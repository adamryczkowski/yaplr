#' @section How a thread is controlled:
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
#' @section How mutexes work
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

#' Initializes cluster
server_init_cluster <- function() {

}

#' Spawns a thread in a host 'host'.
server_spawn_thread<-function(host,expr,start=TRUE, exportlist=list())
{

}

#' Adds server to socket pool. Also initializes parallel managment task on the host
server_add_machine_to_cluster_by_svsocket<-function(hostname, port)
{

}
