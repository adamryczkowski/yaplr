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

#TODO:
#1. Zrób procedurę, która inicjuje struktury danych:
#   a) Tworzy bigmatrix używany do komunikacji
#   b) Tworzy nazwane mutexy: "to_manager" i "to_client"
#   c) Zapisuje to w z góry ustalonym miejscu na dysku

init_client<-function()
{
	.GlobalEnv$buffer_size=4096 #option
	obj<-readRDS('/tmp/yaplr_file.rds')
	.GlobalEnv$.shared_mem<-bigmemory::attach.big.matrix(obj$mem)
	.GlobalEnv$.manager_on_mutex<- synchronicity::boost.mutex('manager_present') #Mutex, który jest zawłaszczony
	#przez działającą pętlę zdarzeń managera. Gdy manager zakończy swoją pętlę, to mutex jest wyzwolony.
	.GlobalEnv$.server_wakeup<-synchronicity::boost.mutex('server_wakeup')
	.GlobalEnv$.idling_manager<-synchronicity::boost.mutex('idling_manager')
	.GlobalEnv$.message_processing<-synchronicity::boost.mutex('message_processing')
	.GlobalEnv$.communication_from_client<-synchronicity::boost.mutex('communication_from_client')
	.GlobalEnv$.communication_to_manager<-synchronicity::boost.mutex('communication_to_manager')

	.GlobalEnv$.shared_mem_guard<-synchronicity::boost.mutex('shared_mem_guard')
}

init_server<-function()
{
	.GlobalEnv$buffer_size=4096 #option
	.GlobalEnv$.shared_mem <- bigmemory::big.matrix(nrow=buffer_size,ncol=1, type='char')
	.GlobalEnv$.manager_on_mutex<- synchronicity::boost.mutex('manager_present')
	.GlobalEnv$.server_wakeup<-synchronicity::boost.mutex('server_wakeup')
	.GlobalEnv$.idling_manager<-synchronicity::boost.mutex('idling_manager')
	.GlobalEnv$.message_processing<-synchronicity::boost.mutex('message_processing')
	.GlobalEnv$.communication_from_client<-synchronicity::boost.mutex('communication_from_client')

	.GlobalEnv$.shared_mem_guard<-synchronicity::boost.mutex('shared_mem_guard')
	saveRDS(list(mem=bigmemory::describe(.GlobalEnv$.shared_mem)), '/tmp/yaplr_file.rds')
	synchronicity::lock(.GlobalEnv$.to_manager_mutex,block=FALSE)
	synchronicity::lock(.GlobalEnv$.server_wakeup)
}

message<-list(msg="Kuku!")

#Funkcja wysyła do serwera objekt 'message'. Serwer wywoła metodę dispatch_message na tym objekcie, aby
#dalej go procesować.
send_to_server<-function(message, block=FALSE)
{
	#Najpierw upewniamy się, że tylko jeden proces w danym momencie stara się wysłać sygnał managerowi
	synchronicity::lock(.GlobalEnv$.communication_to_manager,block=FALSE)
	#W tej chwili tylko my zaczynamy gadać do serwera. Najpierw zawłaszczamy pamięć do rozmowy
	synchronicity::lock(.GlobalEnv$.shared_mem_guard)
	#W tej chwili tylko jeden proces na danej maszynie może mieć dostęp do wspólnej pamięci.
	#
	#Manager być może jest jeszcze zajęty procesowaniem poprzedniego zadania. Poczekamy,
	#aż będzie gotowy, ale najpierw zrobimy naszą część: załadujemy obiekt do pamięci

	obj<-serialize(connection=NULL,message,ascii=FALSE)
	sizeint<-length(serialize(connection=NULL,as.integer(-10)))
	len<-serialize(connection=NULL,length(obj))
	if (length(len)!=sizeint)
	{
		stop("Inconsistent size of integer!")
	}
	.GlobalEnv$.shared_mem[1:sizeint,1]<-len
	if (length(obj)>.GlobalEnv$buffer_size-sizeint)
	{
		browser() #Należy stworzyć nowy shared file tylko na ten objekt
	} else {
		.GlobalEnv$.shared_mem[(sizeint+1):(sizeint+length(obj)),1]<-obj
	}
	synchronicity::unlock(.GlobalEnv$.shared_mem_guard)

	#Czekamy, aż serwer zakończy procesowanie poprzedniego zadania od klienta:
	synchronicity::lock(.GlobalEnv$.communication_from_client)
	synchronicity::unlock(.GlobalEnv$.communication_from_client)
	#Teraz wiemy, że manager jest gotowy, aby obsłużyć nas, bo kanał komunikacyjny jest wolny.
	#
	#Najpierw budzimy managera
	synchronicity::unlock(.GlobalEnv$.server_wakeup) #Obudzony serwer będzie wiedział, że
	#czeka na niego zadanie. Od razu przejdzie do procesowania.
	#
	synchronicity::lock(.GlobalEnv$.idling_manager) #Czekamy, aż manager zwolni zasób "nudzę się"
	#Teraz wiemy, że manager rzeczywiście zaczął procesować naszą wiadomość i zawłaszczył już kanał
	#komunikacji (shared_mem_guard) oraz .message_processing. Możemy zwolnić .communication_to_manager,
	#gdyż z punktu widzenia klienta, cała komunikacja już się skończyła.
	synchronicity::unlock(.GlobalEnv$.idling_manager)

	synchronicity::unlock(.GlobalEnv$.communication_to_manager)

	if (block)
	{
		synchronicity::lock(.GlobalEnv$.message_processing) #Czekamy, aż serwer skończy procesować wiadomość
	}

}

#Pętla musi być wewnątrz tryCatch aby prawidłowo zwolnić mutex 'manager_present'
server_loop<-function()
{
	synchronicity::lock(.GlobalEnv$.server_wakeup)
	#Czeka na nas wiadomość i jest jeden klient, który na nas czeka.
	synchronicity::unlock(.GlobalEnv$.idling_manager) #Już się nie nudzimy
	synchronicity::lock(.GlobalEnv$.message_processing)
	synchronicity::lock(.GlobalEnv$.communication_from_client)

	sizeint<-length(serialize(connection=NULL,as.integer(-10)))
	synchronicity::lock(.GlobalEnv$.shared_mem_guard)
	objsize<-unserialize(connection=as.raw(.GlobalEnv$.shared_mem[1:sizeint,1]))
	if (objsize>.GlobalEnv$buffer_size-sizeint)
	{
		browser()
	} else {
		obj<-unserialize(connection=as.raw(.GlobalEnv$.shared_mem[(sizeint+1):objsize,1]))
	}
	synchronicity::unlock(.GlobalEnv$.shared_mem_guard)
	synchronicity::unlock(.GlobalEnv$.communication_from_client)

	#Mamy objekt obj. Wywołamy teraz kod zawarty w tym obiekcie, albo cokolwiek innego:
	cat(str(obj))

	synchronicity::unlock(.GlobalEnv$.message_processing)
	synchronicity::lock(.GlobalEnv$.idling_manager) #Od tej pory się nudzimy
}
