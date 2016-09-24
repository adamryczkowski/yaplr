#' This method asks server to store the object \code{obj} in storage, so it will be available to the client under a tag
#'
#' obj_storage is a big.matrix with serialized object.
#' tag is a name under which an object will be accessible
remotecall_store_object<-function(obj, tag)
{
	if (class(obj)=='big.matrix.descriptor')
	{
		obj<-bigmemory::attach.big.matrix(obj)
	}
	assign(x=tag, value=list(obj=obj, date=date()), envir=.GlobalEnv$.object_storage)
	return(NULL)
}

#' Retrieves an object from the server.
#' It returns a big.matrix containing the object, or NULL if no object is found
remotecall_retrieve_object<-function(tag)
{
	obj<-.GlobalEnv$.object_storage[[tag]]$obj
	if (class(obj)=='big.matrix')
	{
		obj<-bigmemory::describe(obj)
	}
	return(obj)
}


remotecall_remove_object<-function(tag)
{
	if(exists(tag, envir = .GlobalEnv$.object_storage))
	{
		rm(list = tag, envir = .GlobalEnv$.object_storage)
	}
}

remotecall_does_object_exist<-function(tag)
{
	return(exists(tag, envir = .GlobalEnv$.object_storage))
}

remotecall_ping<-function(noop=NULL)
{
	if (is.null(noop))
	{
		return('pong')
	} else {
		return(object.size(noop))
	}
}

remotecall_list_objects<-function()
{
	if (length(.GlobalEnv$.object_storage)==0)
	{
		return(data.frame(size=numeric(), ctime=character()))
	}

	sizes<-sapply(paste0('.GlobalEnv$.object_storage$', ls(name=.GlobalEnv$.object_storage)),
								function(o)
									length(eval(parse(text=o))$obj)
								)
	names<-ls(name=.GlobalEnv$.object_storage)
	dates<-sapply(paste0('.GlobalEnv$.object_storage$', ls(name=.GlobalEnv$.object_storage)),
								function(o) eval(parse(text=o))$date)
	ans<-data.frame(size=sizes, ctime=dates)
	rownames(ans)<-names
	colnames(ans)<-c('size','ctime')
	return(ans)
}
