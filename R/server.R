



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
	synchronicity::lock(.GlobalEnv$.server_wakeup, block=FALSE)
}

server_loop<-function()
{
	synchronicity::lock(.GlobalEnv$.server_wakeup,block=FALSE)
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
		obj<-unserialize(connection=as.raw(.GlobalEnv$.shared_mem[(sizeint+1):(sizeint+objsize),1]))
	}
	synchronicity::unlock(.GlobalEnv$.shared_mem_guard)
	synchronicity::unlock(.GlobalEnv$.communication_from_client)
	
	#Mamy objekt obj. Wywołamy teraz kod zawarty w tym obiekcie, albo cokolwiek innego:
	cat(str(obj))
	
	synchronicity::unlock(.GlobalEnv$.message_processing)
	synchronicity::lock(.GlobalEnv$.idling_manager) #Od tej pory się nudzimy
}
