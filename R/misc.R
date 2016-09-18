RemoteObject <- R6::R6Class("RemoteObject",
									public = list(
										getobject = function() {
											if (is.null(private$smallobj))
											{
												return(unserialize(connection=extra_storage[,1]))
											} else
											{
												return(private$smallobj)
											}
										}
										),
									private = list(
										smallobj = NULL,
										extra_storage = NULL
									)
)

attach_mutex<-function(name, timeout=NULL)
{
	library(synchronicity)
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
	put_object_in_big_matrix(bm=.GlobalEnv$.shared_mem, obj=NULL)
	synchronicity::unlock(.GlobalEnv$.shared_mem_guard)

	#Server might still be busy serving the asynchronous part of the previous message send by another client.
	#We first need to wait until it finishes with this trick:
	synchronicity::lock(.GlobalEnv$.message_processing) #We wait for the mutex without actually owning it.
	#TODO: We should put a timeout here, so in case server is dead and message_processing is already blocked,
	#we will not deadlock.
	synchronicity::unlock(.GlobalEnv$.message_processing) #I.e. we use this mutex more like a sempahore
	#Now we are sure, that the server is waiting to start serving our request. We only need to wake it up:

	suppressWarnings(flag1<-synchronicity::unlock(.GlobalEnv$.server_wakeup)) #Woken server sees there is nothing in our message
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

#' @export
reset_mutexes<-function()
{
	init_client()

	synchronicity::lock(.GlobalEnv$.shared_mem_guard, block=FALSE)
	synchronicity::unlock(.GlobalEnv$.shared_mem_guard)

	synchronicity::lock(.GlobalEnv$.message_processing, block=FALSE)
	synchronicity::unlock(.GlobalEnv$.message_processing)

}

#' Returns NULL or big.matrix that stores the actual object in case the object is too big to fit bm.
#' Be careful to assign the returned value, otherwise you will get segment violation if gc frees the
#' big.matrix containing the object before it is read.
put_object_in_big_matrix<-function(bm, obj)
{
	sizeint<-length(serialize(connection=NULL,as.integer(-10)))
	if (!is.null(obj))
	{
		rawobj<-serialize(connection=NULL,obj,ascii=FALSE)
	} else{
		rawobj<-raw(0)
	}
	len<-serialize(connection=NULL,length(rawobj))
	if (length(len)!=sizeint)
	{
		stop("Inconsistent size of integer!")
	}
	bm[1:sizeint,1]<-as.raw(len)
	if (length(obj)>nrow(bm)-sizeint)
	{
		extrabm<-bigmemory::big.matrix(nrow=length(obj),ncol=1, type='raw')
		extrabm[,1]<-rawobj
		bm[(sizeint+1):(sizeint+length(obj)),1]<-serialize(connection=NULL,bigmemory::describe(extrabm))
		ans<-extrabm
	} else {
		bm<-rawobj
		ans<-NULL
	}
}

#' Function returns object
get_object_from_big_matrix<-function(bm)
{
	sizeint<-length(serialize(connection=NULL,as.integer(-10)))
	objsize<-unserialize(connection=bm[1:sizeint,1])
	if (objsize > 0)
	{
		ans<-unserialize(connection=bm[(sizeint+1):(objsize+sizeint),1])
		if (objsize>nrow(bm)-sizeint)
		{
			extra_bm<-bigmemory::attach.big.matrix(ans)
			ans<-unserialize(connection=extra_bm)
		}
		return(ans)
	} 	else  {
		return(NULL)
	}

}
