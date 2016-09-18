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

