#' The scope of this package
#' -------------------------
#'
#' As of now, the purpose of the package is to provide a programmer a means to communicate
#' with the other R session on the same machine via shared mutexes and shared memory.
#'
#' The package is compatible with parallel::mcpmcparallel (i.e. forking)

#' Invariants:
#' -----------
#'
#' * Never touches the filesystem. No temporary files. No need to share a folder across a computer.
#'
#' * Don't mess with the forks. User can use forks however they want, but for the purpose of the package, use only
#'   bigmemory::matrix for communication and separate, dedicated process for execution
#'   control (separate for each machine in the cluster)


#' How my mutexes work?
#' --------------------
#'
#' All mutexes are named, so they can be shared by name across the machine.
#' There are 2 kinds of mutexes: local ones (shared across the machine) and global ones (shared globally across the cluster)

#' When a working thread locks a global mutex, what does happen behind the scenes?
#' 1. Calls a function GetGlobalMutex(name).
#' 2. The functions tries to acquire token of communication (another mutex), and when it has one, it pushes request for a
#'    global mutex with the given name. The request gets shared by filling a shared object used as
#'    a buffer for communication
#' 3. The machine manager thread (which is a parent of a spawned )

NULL



#' Spawnes expr as a fork. It returns an object of class 'execution_thread', which allows for
#' control the process.
#' @export
server_spawn_fork<-function(expr, start=TRUE)
{
	stop("Not implemented yet")
}



#' Sends object 'msg' to server. This object will be available to read by the server.
#' Function is non-blocking
#' @export
client_send_message<-function(msg)
{
	stop("Not implemented yet")
}

#' Gets the message from server. If block=TRUE (default) it waits until message is available.
#' If block=FALSE and there is no message, it returns NULL
#' @export
client_receive_message<-function(block=TRUE)
{
	stop("Not implemented yet")
}

#' Creates mutex, usualy used to enforce serialize usage of a critical resource.
#' If distributed=TRUE, than this mutex will be available across all computers in cluster.
#' @export
create_mutex<-function(name=NULL, distributed=FALSE)
{
	if (distributed)
	{
		stop("Distributed mutexes not yet implemented")
	}
	stop("Not implemented yet")

}

#' Sends object 'object' into thread 'thread' under the name 'objectname'. It is fast,
#' if the thread is on the same machine.
#' If block=FALSE and the thread is on the remote host, it only initializes sending process and returns
#' object 'sending_progress' that allows for monitoring the process, and waiting when it has finished.
#' @export
send_object<-function(thread, objectname, object, block=FALSE)
{
	stop("Not implemented yet")

}


message<-list(msg="Kuku!")

#Function sends message 'message' to the server, and optionally waits until server finishes processing it.
#' @export
send_to_server<-function(message, block=FALSE)
{
	#First we make sure, that there is only one process trying to communicate with the server
	synchronicity::lock(.GlobalEnv$.client_is_busy,block=FALSE)
	#Now we can start talking to server. We do this via shared memory, which first needs to be owned:
	synchronicity::lock(.GlobalEnv$.shared_mem_guard)
	#Now we are free to fill the memory with our data.
	#Then, when the memory buffer is ready, we signal the server that we actualy want its attention.
	obj<-serialize(connection=NULL,message,ascii=FALSE)
	sizeint<-length(serialize(connection=NULL,as.integer(-10)))
	len<-serialize(connection=NULL,length(obj))
	if (length(len)!=sizeint)
	{
		stop("Inconsistent size of integer!")
	}
	.GlobalEnv$.shared_mem[1:sizeint,1]<-as.raw(len)
	if (length(obj)>.GlobalEnv$buffer_size-sizeint)
	{
		browser() #We need to create the project elsewhere
	} else {
		.GlobalEnv$.shared_mem[(sizeint+1):(sizeint+length(obj)),1]<-as.raw(obj)
	}
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

	if (block)
	{
		#If user wants us to wait for the completion of the message processing, we wait.
		synchronicity::lock(.GlobalEnv$.message_processing) #Waiting until server finishes processing the message
	}

}

#This is a main event loop for the server. Each iteration of the loop requires the client to unlock the
#'server_wakeup' mutex.
#' @export
server_loop<-function()
{
	if (!is_server_initialized())
	{
		init_server()
	}
	synchronicity::lock(.GlobalEnv$.server_wakeup)
	#Since we are woken up, we know there is a message waiting for us and there is at least one client that
	#waits until we process this message
	synchronicity::unlock(.GlobalEnv$.idling_manager) #End of idling phase
	synchronicity::lock(.GlobalEnv$.message_processing) #Beginning of message processing. Waiting for this
	#mutex allows client to wait until we process the message

	sizeint<-length(serialize(connection=NULL,as.integer(-10)))
	synchronicity::lock(.GlobalEnv$.shared_mem_guard) #We start using the shared memory
	objsize<-unserialize(connection=as.raw(.GlobalEnv$.shared_mem[1:sizeint,1]))
	if (objsize > 0)
	{
		if (objsize>.GlobalEnv$buffer_size-sizeint)
		{
			browser()
		} else {
			obj<-unserialize(connection=as.raw(.GlobalEnv$.shared_mem[(sizeint+1):objsize,1]))
		}
		synchronicity::unlock(.GlobalEnv$.shared_mem_guard)

		#Now we have the obj on server's end. Now we are free to process this object:
		cat(str(obj))

	} 	else  {
		synchronicity::unlock(.GlobalEnv$.shared_mem_guard)
	}
	synchronicity::unlock(.GlobalEnv$.message_processing) #Signaling end of message processing
	synchronicity::lock(.GlobalEnv$.idling_manager) #Signaling beginning of idling
}
