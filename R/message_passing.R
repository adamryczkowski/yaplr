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

attach_mutex<-function(name, timeout=NULL)
{
	ans<-new('boost.mutex.descriptor')
	ans@description<-list(shared.name=name, timeout=timeout)
	m<-synchronicity::attach.mutex(ans)
	suppressWarnings({
		if(synchronicity::lock(m, block=FALSE))
		{
			synchronicity::unlock(m)
		}
	})
	return(m)
}

is_server_initialized<-function()
{
	m<-attach_mutex('server_initialized')
	l<-synchronicity::lock(m,block=FALSE)
	if (l)
	{
		synchronicity::unlock(m)
		return(FALSE)
	}
	return(TRUE)
}

is_client_initialized<-function()
{
	return(!is.null(.GlobalEnv$.shared_mem))
}


#Function that pings the server to ensure it is running and processing the messages.
#It simply sets a proper NULL message to the server, sends it, wakes the server, waits until
#it finishes processing, and then re-checks whether the server is sleeping again.
#If it does, it means it really is running.
#
is_server_running<-function()
{
	if (!is_client_initialized())
	{
		stop("Cannot run when client is not initialized")
	}
	if (!is_server_initialized())
	{
		return(FALSE) #Not initialized server cannot be running
	}

	#Now we try to send the server message with size 0.
	#We proceed the same as with send_message

	synchronicity::lock(.GlobalEnv$.client_is_busy,block=FALSE)
	synchronicity::lock(.GlobalEnv$.shared_mem_guard)
	sizeint<-length(serialize(connection=NULL,as.integer(-10)))
	len<-serialize(connection=NULL,length(obj))
	if (length(len)!=sizeint)
	{
		stop("Inconsistent size of integer!")
	}
	.GlobalEnv$.shared_mem[1:sizeint,1]<-as.raw(as.integer(0))
	synchronicity::unlock(.GlobalEnv$.shared_mem_guard)

	#Server might still be busy serving the asynchronous part of the previous message send by another client.
	#We first need to wait until it finishes with this trick:
	synchronicity::lock(.GlobalEnv$.message_processing) #We wait for the mutex without actually owning it.
	#TODO: We should put a timeout here, so in case server is dead and message_processing is already blocked,
	#we will not deadlock.
	synchronicity::unlock(.GlobalEnv$.message_processing) #I.e. we use this mutex more like a sempahore
	#Now we are sure, that the server is waiting to start serving our request. We only need to wake it up:

	flag1<-synchronicity::unlock(.GlobalEnv$.server_wakeup) #Woken server sees there is nothing in our message
	#then sleeps again
	if (!flag1)
	{
		#Server was not waiting for us!
		synchronicity::unlock(.GlobalEnv$.client_is_busy)
		return(FALSE)
	}
	synchronicity::lock(.GlobalEnv$.message_processing) #We wait until server does this little NO OP thing
	synchronicity::unlock(.GlobalEnv$.message_processing)

	flag2<-synchronicity::lock(.GlobalEnv$.server_wakeup, block=FALSE) #We check whether the server actively locks the
	#server_wakeup
	if (flag2==TRUE)
	{
		#Server did not re-block the server_wakeup flag! It means, server is not responding.
		synchronicity::unlock(.GlobalEnv$.server_wakeup) #We undo the mutex, although it really doesn't matter...
		return(FALSE)
	} else
	{
		return(TRUE)
	}
}


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
}

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
}

message<-list(msg="Kuku!")

#Function sends message 'message' to the server, and optionally waits until server finishes processing it.
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