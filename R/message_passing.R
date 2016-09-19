#' How a thread is controlled?
#' --------------------------
#'
#' When a thread is launched it leaves a global object is a hub of communications with it. The object lives in
#' a global thread of a calling machine and can be accessed remotely.
#'
#' It contains a list of all objects sent from the thread and allows sending information in.
#'
#'
#'
#'
#'
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

#' Invariants:
#'
#' * Never touches the filesystem. No temporary files. No need to share a folder across a computer.
#'
#' * Don't mess with the forks. User can use forks however they want, but for the purpose of the package, use only
#'   bigmemory::matrix for communication and separate, dedicated process for execution
#'   control (separate for each machine in the cluster)
NULL


#' Initializes cluster
server_init_cluster <- function() {

}

#' Spawnes expr as a fork. It returns an object of class 'execution_thread', which allows for
#' control the process.
server_spawn_fork<-function(expr, start=TRUE)
{

}

#' Spawns a thread in a host 'host'.
server_spawn_thread<-function(host,expr,start=TRUE, exportlist=list())
{

}

#' Adds server to socket pool. Also initializes parallel managment task on the host
server_add_machine_to_cluster_by_svsocket<-function(hostname, port)
{

}

#' Sends object 'msg' to server. This object will be available to read by the server.
#' Function is non-blocking
client_send_message<-function(msg)
{

}

#' Gets the message from server. If block=TRUE (default) it waits until message is available.
#' If block=FALSE and there is no message, it returns NULL
client_receive_message<-function(block=TRUE)
{

}

#' Creates mutex, usualy used to enforce serialize usage of a critical resource.
#' If distributed=TRUE, than this mutex will be available across all computers in cluster.
create_mutex<-function(name=NULL, distributed=FALSE)
{

}

#' Sends object 'object' into thread 'thread' under the name 'objectname'. It is fast,
#' if the thread is on the same machine.
#' If block=FALSE and the thread is on the remote host, it only initializes sending process and returns
#' object 'sending_progress' that allows for monitoring the process, and waiting when it has finished.
send_object<-function(thread, objectname, object, block=FALSE)
{

}

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
	synchronicity::lock(.GlobalEnv$.message_processing) #We make sure that server is not idling anymore - i.e. it
	#actually started to process our message. Past this point we can assume server is processing our message.
	synchronicity::unlock(.GlobalEnv$.message_processing) #We use the same non-owning locking
	# of mutex idiom as we did with 'message_processing'.

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

			#We flag that our part of job has ended. All that is left to do is on the part of the server:
			synchronicity::unlock(.GlobalEnv$.client_is_busy)
		} else
		{
			synchronicity::unlock(.GlobalEnv$.client_is_busy)
		}
	} else {
		synchronicity::unlock(.GlobalEnv$.client_is_busy)
	}
	return(ret)
}

