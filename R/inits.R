
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

	return(invisible(NULL))
}

#' @export
init_server<-function()
{
	.GlobalEnv$buffer_size=4096 #option
	.GlobalEnv$.shared_mem <- bigmemory::big.matrix(nrow=buffer_size,ncol=1, type='raw')

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


	saveRDS(list(mem=bigmemory::describe(.GlobalEnv$.shared_mem)), '/tmp/yaplr_file.rds')
	synchronicity::lock(.GlobalEnv$.server_wakeup, block=FALSE)
	synchronicity::lock(.GlobalEnv$.server_initialized, block=FALSE)
	return(invisible(NULL))
}

