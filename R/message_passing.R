
#Function sends message 'message' to the server, and optionally waits until server finishes processing it.
#block implicitely means that we are interested in return value.
send_to_server<-function(method, args, block=FALSE)
{
	#First we make sure, that there is only one process trying to communicate with the server
	synchronicity::lock(.GlobalEnv$.client_is_busy,block=FALSE)
	#Now we can start talking to server. We do this via shared memory, which first needs to be owned:
	synchronicity::lock(.GlobalEnv$.shared_mem_guard)
	#Now we are free to fill the memory with our data.
	#Then, when the memory buffer is ready, we signal the server that we actualy want its attention.
	obj<-list(method=method, args=args)
	#hold_reference is kept to prevent the temporary shared memory that might be created by the
	#'put_object_in_big_matrix' from being destroyed by the gc
	hold_reference<-put_object_in_big_matrix(bm=.GlobalEnv$.shared_mem, obj=obj)
	synchronicity::unlock(.GlobalEnv$.shared_mem_guard)

	#Server might still be busy serving the asynchronous part of the previous message send by another client.
	#We first need to wait until it finishes with this trick:
	synchronicity::lock(.GlobalEnv$.message_processing) #We wait for the mutex without actually owning it.
	#TODO: We should put a timeout here, so in case server is dead and message_processing is already blocked,
	#we will not deadlock.
	synchronicity::unlock(.GlobalEnv$.message_processing) #I.e. we use this mutex more like a sempahore
	#Now we are sure, that the server is waiting to start serving our request. We only need to wake it up:
	synchronicity::unlock(.GlobalEnv$.server_wakeup) #Woken server starts to deserialize our message and then proceeds
	#to process it.
	#
	synchronicity::lock(.GlobalEnv$.idling_manager) #We make sure that server is not idling anymore - i.e. it
	#actually started to process our message. Past this point we can assume server is processing our message.
	synchronicity::unlock(.GlobalEnv$.idling_manager) #We use the same non-owning locking
	# of mutex idiom as we did with 'message_processing'.

	#We flag that our part of job has ended. All that is left to do is on the part of the server:
	synchronicity::unlock(.GlobalEnv$.client_is_busy)
	ret<-NULL
	if (block || !is.null(hold_reference))
	{
		#If user wants us to wait for the completion of the message processing, we wait.
		synchronicity::lock(.GlobalEnv$.message_processing) #Waiting until server finishes processing the message

		if (!is.null(hold_reference))
		{
			rm(hold_reference)
		}

		if (block)
		{
			synchronicity::lock(.GlobalEnv$.shared_mem_guard)
			ret<-get_object_from_big_matrix(.GlobalEnv$.shared_mem)
			synchronicity::unlock(.GlobalEnv$.shared_mem_guard)
		}
	}
	return(ret)
}

