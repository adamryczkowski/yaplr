#This is a main event loop for the server. Each iteration of the loop requires the client to unlock the
#'server_wakeup' mutex.
server_loop<-function(quiet=FALSE)
{
	if (!is_server_initialized())
	{
		init_server()
	}
	exit_flag=FALSE
	synchronicity::unlock(.GlobalEnv$.server_initializing)
	while(!exit_flag)
	{

		synchronicity::lock(.GlobalEnv$.idling_server)
		synchronicity::lock(.GlobalEnv$.server_wakeup)
		#Since we are woken up, we know there is a message waiting for us and there is at least one client that
		#waits until we process this message
		synchronicity::lock(.GlobalEnv$.message_processing) #Beginning of message processing. Waiting for this
		#mutex allows client to wait until we process the message
		synchronicity::unlock(.GlobalEnv$.idling_server)

		obj<-get_object_from_big_matrix(.GlobalEnv$.shared_mem)
		hold_reference<-NULL

		if (!is.null(obj))
		{
			if (obj$method == 'quit')
			{
				exit_flag=TRUE
				if (!quiet)
					cat("Received 'quit' command\n")
			} else
			{
				if (!quiet)
					cat(paste0("Received '", obj$method, "' command\n"))

				ans<-call_function(obj)
				if (!is.null(ans))
				{
					synchronicity::lock(.GlobalEnv$.shared_mem_guard)
					hold_reference<-put_object_in_big_matrix(bm = .GlobalEnv$.shared_mem, obj = ans)
					synchronicity::unlock(.GlobalEnv$.shared_mem_guard)
				} else
				{
					synchronicity::lock(.GlobalEnv$.shared_mem_guard)
					hold_reference<-put_object_in_big_matrix(bm = .GlobalEnv$.shared_mem, obj = NULL)
					synchronicity::unlock(.GlobalEnv$.shared_mem_guard)
				}
			}
		}


		if (!is.null(hold_reference))
		{
			#Waiting until client finishes processing the request
			synchronicity::lock(.GlobalEnv$.client_is_busy)
			synchronicity::unlock(.GlobalEnv$.client_is_busy)
		}

		synchronicity::unlock(.GlobalEnv$.message_processing) #Signaling end of message processing
		#cat('Server has received a message!\n')
	}
}

call_function<-function(obj)
{
	do.call(eval(parse(text = paste0('yaplr:::remotecall_',obj$method))),args=obj$args, envir=.GlobalEnv)
}
