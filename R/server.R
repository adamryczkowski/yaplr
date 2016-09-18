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

		sizeint<-length(serialize(connection=NULL,as.integer(-10)))
		synchronicity::lock(.GlobalEnv$.shared_mem_guard) #We start using the shared memory
		objsize<-unserialize(connection=.GlobalEnv$.shared_mem[1:sizeint,1])
		if (objsize > 0)
		{
			if (objsize>.GlobalEnv$buffer_size-sizeint)
			{
				browser()
			} else {
				obj<-unserialize(connection=.GlobalEnv$.shared_mem[(sizeint+1):(objsize+sizeint),1])
			}
			synchronicity::unlock(.GlobalEnv$.shared_mem_guard)

			#Now we have the obj on server's end. Now we are free to process this object:
			cat(str(obj))

		} 	else  {
			synchronicity::unlock(.GlobalEnv$.shared_mem_guard)
		}
		synchronicity::unlock(.GlobalEnv$.message_processing) #Signaling end of message processing
		synchronicity::lock(.GlobalEnv$.idling_manager, block=FALSE) #Signaling beginning of idling
		cat('Server has received a message!\n')
	}
}
