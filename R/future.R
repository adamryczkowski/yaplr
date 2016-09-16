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
