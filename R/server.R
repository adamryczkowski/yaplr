#This is a main event loop for the server. Each iteration of the loop requires the client to unlock the
#'server_wakeup' mutex.
#' @export
server_loop<-function()
{
	if (!is_server_initialized())
	{
		init_server()
	}
	exit_flag=FALSE
	while(!exit_flag)
	{

		synchronicity::lock(.GlobalEnv$.server_wakeup)
		#Since we are woken up, we know there is a message waiting for us and there is at least one client that
		#waits until we process this message
		synchronicity::unlock(.GlobalEnv$.idling_manager) #End of idling phase
		synchronicity::lock(.GlobalEnv$.message_processing) #Beginning of message processing. Waiting for this
		#mutex allows client to wait until we process the message

		obj<-get_object_from_big_matrix(.GlobalEnv$.shared_mem)

		if (!is.null(obj))
		{
			ans<-call_function(obj)
			if (!is.null(ans))
			{
				put_object_in_big_matrix(bm = .GlobalEnv$.shared_mem, obj = ans)
			} else
			{
				put_object_in_big_matrix(bm = .GlobalEnv$.shared_mem, obj = NULL)
			}
		}

		synchronicity::unlock(.GlobalEnv$.shared_mem_guard)


		synchronicity::unlock(.GlobalEnv$.message_processing) #Signaling end of message processing
		synchronicity::lock(.GlobalEnv$.idling_manager, block=FALSE) #Signaling beginning of idling
		cat('Server has received a message!\n')
	}
}

call_function<-function(obj)
{
	do.call(eval(parse(text = paste0('yaplr:::remotecall_',obj$method))),args=obj$args, envir=.GlobalEnv)
}
