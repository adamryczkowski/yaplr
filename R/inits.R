#' @title Initializes and shuts down the client
#' @description \code{init_client} has to be called before any client function can be run.
#' It is safe to run this function many times, however server side must be
#' initialized first.
#'
#' \code{shutdown_client} closes (frees) all mutexes and frees the small shared memory buffer on the
#' client side. Nothing will happen if you don't call it except leaked mutexes.
#'
#' @export
init_client<-function()
{
	if (!is_server_initialized())
	{
		stop("Cannot initialize client before initialization of the server.")
	}
	.GlobalEnv$buffer_size=4096 #option
	obj<-readRDS('/tmp/yaplr_file.rds')
	.GlobalEnv$.shared_mem<-bigmemory::attach.big.matrix(obj$mem)
	.GlobalEnv$.server_wakeup<-attach_mutex('server_wakeup')
	.GlobalEnv$.server_initialized<-attach_mutex('server_initialized')
	.GlobalEnv$.idling_manager<-attach_mutex('idling_manager')
	.GlobalEnv$.message_processing<-attach_mutex('message_processing')
	.GlobalEnv$.client_is_busy<-attach_mutex('client_is_busy')

	.GlobalEnv$.shared_mem_guard<-synchronicity::boost.mutex('shared_mem_guard')

	#Storage for pointers (big.matrices) to stored objects

	return(invisible(NULL))
}

#' @rdname init_client
#' @export
shutdown_client<-function()
{
	symbols<-c('buffer_size','.shared_mem','.server_wakeup','.server_initialized','.idling_manager','.message_processing', '.client_is_busy', '.shared_mem_guard')

	rm(list = symbols, envir = .GlobalEnv)
	gc() #Free the mutexes and shared memeory.
}

#' @title Resets all communication mutexes.
#'
#' @description When you are sure that no communication takes place in the background
#' between any clients and the server this function is safe to run.
#'
#' It ensures that no deadlocks will emerge during subsequent communication with the server.
#'
#' The presence of this function is mostly for debugging purposes. Once the package leaves the
#' beta stage, it will removed.
#' @export
reset_communication<-function()
{
	if (exists('.client_is_busy',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.client_is_busy))
	}

	if (exists('.message_processing',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.message_processing))
	}

		if (exists('.shared_mem_guard',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.shared_mem_guard))
	}

}


#' @title Initializes and shuts down the server
#' @description It initializes mutexes and shared memory that are used for communication.
#'
#' There can be only one server running on a given machine.
#'
#' When you no longer need the server (after the server exited its message loop), run \code{shutdown_server()}

#' @export
init_server<-function(force=FALSE)
{
	if (exists(x = '.yaplr_role', envir=.GlobalEnv))
	{
		if (.GlobalEnv$.yaplr_role != 'client')
		{
			stop("This session is already initialized as client. Shutdown the client first!")
		}
	}

	if (!force && is_server_initialized())
	{
		if (!exists(x = '.yaplr_role', envir=.GlobalEnv))
		{
			stop("The server seems to be already initialized elsewhere. To force initializing the server
					 here, add a 'force=TRUE' parameter to 'init_server' function call")
		}
	}

	if (is_server_running())
	{
		stop("The server is already running somewhere on the machine. You cannot have more than 2 servers running
				 simultaneously on one server. Use the existing server instead.")
	}
	.GlobalEnv$buffer_size=4096 #option
	.GlobalEnv$.shared_mem <- bigmemory::big.matrix(nrow=.GlobalEnv$buffer_size,ncol=1, type='raw')

	#This mutex can be owned only by server and freed only by client.
	.GlobalEnv$.server_wakeup<-synchronicity::boost.mutex('server_wakeup')

	#This mutex is owned by server and freed by server. It is used as a cross-process flag indicating
	#initialized shared memory
	.GlobalEnv$.server_initialized<- synchronicity::boost.mutex('server_initialized')

	#This mutex is owned by server and freed by server. It is locked when server is idle, and
	#free when server processes a message. It is used as a cross-process flag indicating whether server
	#is busy
	.GlobalEnv$.idling_manager<-synchronicity::boost.mutex('idling_manager')

	#This mutex is owned by server and freed by server. It is locked when server processes a message.
	#It is used as a cross-process flag indicating whether server is busy
	.GlobalEnv$.message_processing<-synchronicity::boost.mutex('message_processing')

	#This mutex is guarding the shared memory. Both server and client can own it.
	#It is one of the the few mutexes that perform a role a mutex was designed for ;-)
	.GlobalEnv$.shared_mem_guard<-synchronicity::boost.mutex('shared_mem_guard')

	#This mutex is owned by client. It is locked when client is in process of preparing and sending information
	#to the server (shared memory).
	#The mutex ensures that only one client can talk to the server at a time.
	.GlobalEnv$.client_is_busy<-synchronicity::boost.mutex('client_is_busy')

	.GlobalEnv$.object_starage<-new.env(parent=emptyenv(), hash=TRUE)

	saveRDS(list(mem=bigmemory::describe(.GlobalEnv$.shared_mem)), '/tmp/yaplr_file.rds')
	synchronicity::lock(.GlobalEnv$.server_wakeup, block=FALSE)
	synchronicity::lock(.GlobalEnv$.server_initialized, block=FALSE)

	suppressWarnings({synchronicity::lock(.GlobalEnv$.idling_manager, block=FALSE);
									synchronicity::unlock(.GlobalEnv$.idling_manager)})

	return(invisible(NULL))
}

#' @rdname init_server
#' @export
shutdown_server<-function()
{
	reset_communication()

	symbols<-c('buffer_size','.shared_mem','.server_wakeup','.server_initialized','.idling_manager','.message_processing', '.client_is_busy', '.shared_mem_guard')

	if (exists(x = '.server_initialized',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.server_initialized))
	}

	rm(list = symbols, envir = .GlobalEnv)
	if (file.exists('/tmp/yaplr_file.rds'))
	{
		unlink('/tmp/yaplr_file.rds')
	}

	if (exists('.server_wakeup',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.server_wakeup))
	}
	if (exists('.idling_manager',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.idling_manager))
	}
	if (exists('.server_initialized',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.server_initialized))
	}

}
