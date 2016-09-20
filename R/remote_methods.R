#' This method asks server to store the object \code{obj} in storage, so it will be available to the client under a tag
#'
#' obj_storage is a big.matrix with serialized object.
#' tag is a name under which an object will be accessible
remotecall_store_object<-function(obj, tag)
{
	assign(x=tag, value=obj, envir=.GlobalEnv$.object_starage)
	return(NULL)
}

#' Retrieves an object from the server.
#' It returns a big.matrix containing the object, or NULL if no object is found
remotecall_retrieve_object<-function(tag)
{
	return(.GlobalEnv$.object_starage[tag])
}


remotecall_free_object<-function(tag)
{
	if(exists(tag, envir = .GlobalEnv$.object_starage))
	{
		rm(list = tag, envir = .GlobalEnv$.object_starage)
	}
}

remotecall_ping<-function()
{
	return('pong')
}
