#' @title Initializes and shuts down the client
#' @description \code{init_client} has to be called before any client function can be run.
#' It is safe to run this function many times, however server side must be
#' initialized first.
#'
#' \code{shutdown_client} closes (frees) all mutexes and frees the small shared memory buffer on the
#' client side. Nothing will happen if you don't call it except leaked mutexes.
#'
#' @rdname init_client
init_client<-function(server_ok=FALSE)
{
	if (exists(x = '.role', envir = .GlobalEnv))
	{
		role = .GlobalEnv$.role
	} else
	{
		role = ''
	}

	if (role=='server')
	{
		if (server_ok)
		{
			return(FALSE) #No need to initialize server as client.
		} else {
			stop("Cannot initialize client on server")
		}
	}

	if (!is_server_initialized())
	{
		stop("Cannot initialize client before initialization of the server.")
	}
	shared_file<-getOption('yaplr_shared_file')
	if (!file.exists(shared_file))
	{
		stop("The shared file is missing. Is server really running?")
	}
	if (exists(x='.bm_mtime', envir = .GlobalEnv))
	{
		if (file.mtime(shared_file) == .GlobalEnv$.bm_mtime)
		{
			return(FALSE)
		}
	}
	.GlobalEnv$buffer_size=4096 #option
	obj<-readRDS(shared_file)
	.GlobalEnv$.bm_mtime<-file.mtime(shared_file)
	.GlobalEnv$.shared_mem<-bigmemory::attach.big.matrix(obj$mem)
	.GlobalEnv$.server_wakeup<-attach_mutex('server_wakeup')
	.GlobalEnv$.server_initialized<-attach_mutex('server_initialized')
	.GlobalEnv$.server_initializing<-attach_mutex('server_initializing')
	.GlobalEnv$.idling_server<-attach_mutex('idling_server')
	.GlobalEnv$.message_processing<-attach_mutex('message_processing')
	.GlobalEnv$.client_is_busy<-attach_mutex('client_is_busy')
	.GlobalEnv$.role<-'client'


	.GlobalEnv$.shared_mem_guard<-attach_mutex('shared_mem_guard')

	#Storage for pointers (big.matrices) to stored objects

	return(invisible(role!='client'))
}

#' @rdname init_client
shutdown_client<-function(server_ok=FALSE)
{
	if (exists(x = '.role', envir = .GlobalEnv))
	{
		role <- .GlobalEnv$.role
	} else
	{
		role <- ''
	}
	if (role == 'server')
	{
		if (server_ok)
		{
			return(invisible(FALSE)) #Uninitializing client on server does nothing
		} else {
			warning("Shutting down client on server does nothing")
		}
	}
	symbols<-c('buffer_size','.shared_mem','.server_wakeup','.server_initialized','.idling_manager','.message_processing', '.client_is_busy', '.idling_server', '.shared_mem_guard', '.role')

	suppressWarnings(rm(list = symbols, envir = .GlobalEnv))
	gc() #Free the mutexes and shared memeory.
	if (role!='client')
	{
		warning("Client was not initialized anyway, no need to shut down")
	}
	invisible(role=='client')
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
reset_communication<-function()
{
	if (exists('.client_is_busy',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.client_is_busy))
	} else {
		m<-attach_mutex('client_is_busy')
		suppressWarnings(synchronicity::unlock(m))
	}

	if (exists('.message_processing',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.message_processing))
	} else {
		m<-attach_mutex('message_processing')
		suppressWarnings(synchronicity::unlock(m))
	}

	if (exists('.shared_mem_guard',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.shared_mem_guard))
	} else {
		m<-attach_mutex('shared_mem_guard')
		suppressWarnings(synchronicity::unlock(m))
	}

	if (exists('.server_initializing',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.server_initializing))
	} else {
		m<-attach_mutex('server_initializing')
		suppressWarnings(synchronicity::unlock(m))
	}
}


#' @title Initializes and shuts down the server
#' @description It initializes mutexes and shared memory that are used for communication.
#'
#' There can be only one server running on a given machine.
#'
#' When you no longer need the server (after the server exited its message loop), run \code{shutdown_server()}

#' @rdname init_server
init_server<-function(force=FALSE)
{
	if (exists(x = '.role', envir = .GlobalEnv))
	{
		role <- .GlobalEnv$.role
	} else {
		role <- ''
	}

	if (role == 'client')
	{
		stop("This session is already initialized as client. Shutdown the client first!")
	}

	if (is_server_initialized())
	{
		if (is_server_running())
		{
			if (!force)
			{
				stop("The server is already initialized and running somewhere on the machine. You cannot use more than one server on a single machine. Shut down the old server.")
			} else {
				warning("The server is already initialized and running somewhere on the machine. You cannot use more than one server on a single machine. Shut down the old server.")
			}
		} else {
			if (role == 'server')
			{
				if (!force)
				{
					warning("The server seems to be already initialized here.")
				}
			} else {
				if (!force)
				{
					stop("Server was already initialized somewhere else. To force re-initialization of the server here (which effectively prevents the old server from accepting the messages unless he re-initializes himself) add 'force=TRUE' parameter to 'init_server' call.")
				}
			}
		}
	}

	shared_file<-getOption('yaplr_shared_file')
	.GlobalEnv$buffer_size=4096 #option
	.GlobalEnv$.role='server'
	.GlobalEnv$.shared_mem <- bigmemory::big.matrix(nrow=.GlobalEnv$buffer_size,ncol=1, type='raw')

	#This mutex can be owned only by server and freed only by client.
	.GlobalEnv$.server_wakeup<-attach_mutex('server_wakeup')
	synchronicity::lock(.GlobalEnv$.server_wakeup, block=FALSE)

	#This mutex is owned by server and freed by server. It is used as a cross-process flag indicating
	#initialized shared memory. .server_initializing works in reverse, i.e. it is actually freed by the server
	.GlobalEnv$.server_initialized<-attach_mutex('server_initialized')
	.GlobalEnv$.server_initializing<-attach_mutex('server_initializing')
	suppressWarnings(synchronicity::lock(.GlobalEnv$.server_initializing, block=FALSE))

	#This mutex is owned by server and freed by server. It is locked when server is idle, and
	#free when server processes a message. It is used as a cross-process flag indicating whether server
	#is busy
	.GlobalEnv$.idling_server<-attach_mutex('idling_server')
	suppressWarnings(synchronicity::unlock(.GlobalEnv$.idling_server))

	#This mutex is owned by server and freed by server. It is locked when server processes a message.
	#It is used as a cross-process flag indicating whether server is busy
	.GlobalEnv$.message_processing<-attach_mutex('message_processing')
	suppressWarnings(synchronicity::unlock(.GlobalEnv$.message_processing))

	#This mutex is guarding the shared memory. Both server and client can own it.
	#It is one of the the few mutexes that perform a role a mutex was designed for ;-)
	.GlobalEnv$.shared_mem_guard<-attach_mutex('shared_mem_guard')
	suppressWarnings(synchronicity::unlock(.GlobalEnv$.shared_mem_guard))

	#This mutex is owned by client. It is locked when client is in process of preparing and sending information
	#to the server (shared memory).
	#The mutex ensures that only one client can talk to the server at a time.
	.GlobalEnv$.client_is_busy<-attach_mutex('client_is_busy')
	suppressWarnings(synchronicity::unlock(.GlobalEnv$.client_is_busy))

	.GlobalEnv$.object_storage<-new.env(parent=emptyenv(), hash=TRUE)


	saveRDS(list(mem=bigmemory::describe(.GlobalEnv$.shared_mem)), shared_file)

#	suppressWarnings({synchronicity::lock(.GlobalEnv$.idling_manager, block=FALSE);
#									synchronicity::unlock(.GlobalEnv$.idling_manager)})

	synchronicity::lock(.GlobalEnv$.server_initialized, block=FALSE)
	return(invisible(role!='server'))
}

#' @rdname init_server
shutdown_server<-function()
{
	if (exists(x = '.role', envir = .GlobalEnv))
	{
		is_server = .GlobalEnv$.role == 'server'
	} else
	{
		is_server=FALSE
	}

	reset_communication()

	symbols<-c('buffer_size','.shared_mem','.server_wakeup','.server_initialized','.idling_manager','.message_processing', '.idling_server', '.client_is_busy', '.shared_mem_guard', '.role')

	if (exists(x = '.server_initialized',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.server_initialized))
	} else {
		m<-attach_mutex('server_initialized')
		suppressWarnings(synchronicity::unlock(m))
	}

	shared_file<-getOption('yaplr_shared_file')
	if (file.exists(shared_file))
	{
		unlink(shared_file)
	}

	if (exists('.server_wakeup',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.server_wakeup))
	}
	# if (exists('.idling_manager',envir=.GlobalEnv))
	# {
	# 	suppressWarnings(synchronicity::unlock(.GlobalEnv$.idling_manager))
	# }
	if (exists('.server_initialized',envir=.GlobalEnv))
	{
		suppressWarnings(synchronicity::unlock(.GlobalEnv$.server_initialized))
	}
	suppressWarnings(rm(list = symbols, envir = .GlobalEnv))
	gc() #Free the mutexes and shared memeory.
	invisible(is_server)
}
