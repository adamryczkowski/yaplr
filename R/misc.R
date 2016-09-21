
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

#' @export
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

#' @export
is_client_initialized<-function()
{

	if(is.null(.GlobalEnv$.shared_mem))
		return (FALSE)

	shared_file<-getOption('shared_file')
	if (!file.exists(shared_file))
	{
		return(FALSE)
	}
	if (exists(x='.bm_mtime', envir = .GlobalEnv))
	{
		if (file.mtime(shared_file) == .GlobalEnv$.bm_mtime)
		{
			return(TRUE)
		}
	}
	return(FALSE)
}

#' @title Tests if server is processing messages
#' @description  Function that pings the server to ensure it is running and processing the messages.
#' It simply sets a proper NULL message to the server, sends it, wakes the server, waits until
#' it finishes processing, and then re-checks whether the server is sleeping again.
#' If it does, it means it really is running.
#' @export
is_server_running<-function()
{
	if (!is_server_initialized())
	{
		return(FALSE) #Not initialized server cannot be running
	}

	#Now we try to check whether the server waits for the messages

	#First we wait until the server is not busy

	if (exists(x = '.message_processing', envir = .GlobalEnv))
	{
		mutex_message_processing<-.GlobalEnv$.message_processing
	} else {
		mutex_message_processing<-attach_mutex('message_processing')
	}

	if (exists(x='.idling_server', envir=.GlobalEnv))
	{
		mutex_idling_server<-.GlobalEnv$.idling_server
	} else {
		mutex_idling_server<-attach_mutex('idling_server')
	}

	synchronicity::lock(mutex_message_processing) #We wait for the mutex without actually owning it.
	#Now we check idling state

	suppressWarnings(not_idling<-synchronicity::lock(mutex_idling_server, block=FALSE))
	if (not_idling)
	{
		synchronicity::unlock(mutex_idling_server)
		is_up<-FALSE
	} else {
		is_up<-TRUE
	}

	synchronicity::unlock(mutex_message_processing) #Stop blocking the server
	return(is_up)
}


#' @title Puts object in a shared big.matrix
#'
#' @param bm big.matrix where the data should go
#' @param obj obj to store. If it is larger than a buffor, it will be stored in a separate big.matrix
#' @param payload large object that will not get deserialized unless the method explicitly asks for it.
#'
#' @description Returns NULL or big.matrix that stores the actual object in case the object is too big to fit bm.
#' Be careful to assign the returned value, otherwise you will get segment violation if gc frees the
#' big.matrix containing the object before it is read.
put_object_in_big_matrix<-function(bm, obj)
{
	sizeint<-length(serialize(connection=NULL,as.integer(-10)))
	if (!is.null(obj))
	{
		#This is short path where obj is already a big.matrix.
		#In such case allocation of big.matrix and serialization is not necessary.
		#We still return the big.matrix (i.e. obj) for consistency.
		if (class(obj)=='big.matrix')
		{
			raw_descr_bm<-serialize(connection=NULL,bigmemory::describe(obj))
			bm[(sizeint+1):(sizeint+length(raw_descr_bm)),1]<-raw_descr_bm
			ans<-obj
			return(ans)
		} else {
			rawobj<-serialize(connection=NULL,obj,ascii=FALSE)
		}
	} else{
		rawobj<-raw(0)
	}
	len<-serialize(connection=NULL,length(rawobj))
	if (length(len)!=sizeint)
	{
		stop("Inconsistent size of integer!")
	}
	bm[1:sizeint,1]<-as.raw(len)
	if (length(rawobj)>nrow(bm)-sizeint)
	{
		extrabm<-bigmemory::big.matrix(nrow=length(obj),ncol=1, type='raw')
		extrabm[,1]<-rawobj
		rawbm<-serialize(connection=NULL,bigmemory::describe(extrabm))
		bm[(sizeint+1):(sizeint+length(rawbm)),1]<-rawbm
		ans<-extrabm
	} else {
		if (length(rawobj)>0)
		{
			bm[(sizeint+1):(sizeint+length(rawobj)),1]<-rawobj
		}
		ans<-NULL
	}
}

#' Function that does not deserialize large objects. It returns either an object, or
#' big.matrix with the unserialized object
get_object_from_big_matrix_raw<-function(bm)
{
	sizeint<-length(serialize(connection=NULL,as.integer(-10)))
	objsize<-unserialize(connection=bm[1:sizeint,1])
	if (objsize > 0)
	{
		ans<-unserialize(connection=bm[(sizeint+1):(objsize+sizeint),1])
		if (objsize>nrow(bm)-sizeint)
		{
			ans<-bigmemory::attach.big.matrix(ans)
		}
		return(ans)
	} 	else  {
		return(NULL)
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
			rm(extra_bm)
			gc()
		}
		return(ans)
	} 	else  {
		return(NULL)
	}
}
