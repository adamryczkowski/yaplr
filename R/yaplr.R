#' Functions for fast send & receive objects via shared memory between different E processes in the same
#' machine.
#'
#' @name yaplr-package
#' @aliases yaplr-package yaplr
#' @docType package
#'
#' @author Adam Ryczkowski
#'
#' Maintainer: Adam Ryczkowski <adam@statystyka.net>
#'
NULL

# nocov start
.onLoad	<-	function(libname,	pkgname)	{
	op	<-	options()
	op.yaplr	<-	list(
		yaplr_buffer_size	=	4096,
		yaplr_automatic_server_start	=	TRUE,
		yaplr_default_server_type	=	"rscript",
		yaplr_shared_file = '/tmp/yaplr_file.rds' #TODO: Different file on Windows
	)
	toset	<-	!(names(op.yaplr)	%in%	names(op))
	if(any(toset))	options(op.yaplr[toset])
	invisible()
}
# nocov end


#' @title Makes sure the server is started
#' @description Makes sure the server
#' @return \code{FALSE} if server was already running, \code{TRUE} otherwise.
#' @export
make_sure_server_is_started<-function(server_type=NULL, quiet=FALSE)
{
	if (is.null(server_type))
	{
		server_type=getOption('yaplr_default_server_type')
	}
	if (!server_type %in% c('fork','rscript', 'null'))
	{
		stop("Server type can either be 'fork' (invalid on Windows) or 'rscript'. You can use the option 'default_server_type' to set the default server type.")
	}
	if (is_server_running())
	{
		if (ping_server(quiet = TRUE))
		{
			return(FALSE)
		}
	}
	servercode<-c(
		'library(yaplr)',
		'yaplr:::init_server()',
		'yaplr:::server_loop(quiet=TRUE)',
		'yaplr:::shutdown_server()'
	)
	if (!exists(x = '.server_initializing', envir = .GlobalEnv ))
	{
		.GlobalEnv$.server_initializing<-attach_mutex('server_initializing')
	}
	suppressWarnings(synchronicity::lock(.GlobalEnv$.server_initializing, block=FALSE))
	if (server_type=='fork')
	{
		fn<-function(){}
		body(fn)<-parse(text=paste0(c('{',servercode,'}')))
		parallel::mcparallel(expr=fn,detached = TRUE)
		if (!quiet)
			message('Server process spawned via fork')
	} else if (server_type == 'rscript') 	{
		tmp<-tempfile(pattern = 'server_script_',fileext='.R')
		writeLines(text=servercode, con=tmp)
		system(paste0('Rscript ',tmp),wait = FALSE)
		if (!quiet)
			message('Server process spawned')
	}
	#Here we wait for server starting the message loop, or... wait indefinitely
	#TODO timeout here
	synchronicity::lock(.GlobalEnv$.server_initializing)
	return(TRUE)
}

#' @title Makes sure the server is down
#' @description This function tries to shutdown the server
#' @return \code{TRUE} if server was brought down. \code{FALSE} if server wasn't running anyway.
#'
#' @export
make_sure_server_is_down<-function()
{
	if (is_server_running())
	{
		if (!is_client_initialized())
		{
			init_client(server_ok = FALSE)
		}
		send_to_server('quit',NULL)
		return(TRUE)
	}
	return(FALSE)
}

#' @title Makes sure client-server system is initialized
#' @description If server is off, it first automatically starts it (if \code{automatic_server_start=TRUE},
#' otherwise throws an error), then makes sure this session is connected to it. Optionally checks connectivity
#' with ping
auto_init<-function(automatic_server_start=NULL, check_server_with_ping=TRUE, server_type=NULL)
{
	if (is.null(automatic_server_start))
	{
		automatic_server_start<-getOption('yaplr_automatic_server_start')
	}
	if (!is_server_initialized())
	{
		if (automatic_server_start)
		{
			make_sure_server_is_started(server_type=server_type)
		} else {
			stop("No server is available. Start server with 'make_sure_server_is_started' first.")
		}
	}

	if (!is_client_initialized())
		init_client(server_ok=FALSE)

	if (check_server_with_ping)
	{
		if (!ping_server(quiet=TRUE))
		{
			stop("Server is not responding to pings")
		}
	} else{
		if (!is_server_running())
		{
			stop("Server does not seem to be running the message queue")
		}
	}

	return(invisible(NULL))
}

#' @title Pings the server. The ultimate way to check whether server is running
#' @description Pulls and pushes back a replay of a 'ping' command. If there are problems reaching the server
#' warnings are issued (unless \code{quiet=TRUE}).
#' @export
ping_server<-function(quiet=FALSE)
{
	auto_init(automatic_server_start = FALSE, check_server_with_ping = FALSE)

	if (!is_server_running())
	{
		if (!quiet)
		{
			warning("Server is not running the message loop")
		}
		return(FALSE)
	}

	ans<-send_to_server(method='ping',args=list())
	if (identical(ans,'pong'))
	{
		return(TRUE)
	} else {
		return(FALSE)
	}
}

#' @title Manages a copy of an object to the server's temporary storage.
#' @description After sending, the object will be later available for all clients to retrieve.
#' If you no longer need the object to be kept, ask server to remove it with 'remove_object'.
#' You can check the existence of the object via 'does_object_exist' method.
#'
#' If any method does not successfully finish an error is thrown.
#'
#' @param obj Object to store. The object will get serialized and pushed to the shared memory.
#' @param tag The name used to address (identify) the object on the server.
#' @param automatic_server_start If \code{TRUE}, (either option or argument to this function)
#' and no sign of the server instance is found on this machine, then the server will be
#' launched in background using the default settings.
#' @param check_server_with_ping If \code{TRUE}, the connection with the server will be probed with
#' \code{ping} prior to sending the object. It some runtime overhead, so when sending multple small objects
#' one might want to turn this option off.
#' @export
#' @rdname object_management
send_object<-function(obj, tag, automatic_server_start=NULL, check_server_with_ping=TRUE, force_unserialize_on_server=FALSE)
{
	auto_init(automatic_server_start = automatic_server_start, check_server_with_ping = check_server_with_ping)

	if (!force_unserialize_on_server)
	{
		rawobj<-serialize(object =  obj,connection = NULL)


		objbm<-bigmemory::big.matrix(nrow=length(rawobj), ncol=1, type='raw')
		objbm[,1]<-rawobj
		objdescr<-bigmemory::describe(objbm)
	} else {
		objdescr=obj
	}


	send_to_server('store_object', list(obj=objdescr, tag=tag))
	return(invisible(NULL))
}

#' @export
#' @rdname object_management
retrieve_object<-function(tag, automatic_server_start=NULL, check_server_with_ping=TRUE)
{
	auto_init(automatic_server_start = automatic_server_start, check_server_with_ping = check_server_with_ping)

	ans<-send_to_server('retrieve_object', list(tag=tag))
	if (class(ans)=="big.matrix.descriptor")
	{
		bm<-bigmemory::attach.big.matrix(ans)
		ans<-unserialize(connection = bm[,1])
	}
	return(ans)
}

#' @export
#' @rdname object_management
remove_object<-function(tag, automatic_server_start=NULL, check_server_with_ping=TRUE)
{
	auto_init(automatic_server_start = automatic_server_start, check_server_with_ping = check_server_with_ping)

	send_to_server('remove_object', list(tag=tag))
	return(invisible(NULL))
}

#' @export
#' @rdname object_management
does_object_exist<-function(tag, automatic_server_start=NULL, check_server_with_ping=TRUE)
{
	auto_init(automatic_server_start = automatic_server_start, check_server_with_ping = check_server_with_ping)

	ans<-send_to_server('does_object_exist', list(tag=tag))
	return(ans)
}

#' @export
#' @rdname object_management
list_objects<-function(automatic_server_start=NULL, check_server_with_ping=TRUE)
{
	auto_init(automatic_server_start = automatic_server_start, check_server_with_ping = check_server_with_ping)

	ans<-send_to_server(method='list_objects', args=list())
	return(ans)
}

#' @export
quit_server<-function(check_server_with_ping=TRUE)
{
	if (is_server_running())
	{
		send_to_server(method='quit', args=NULL)
		return(TRUE)
	}
	return(FALSE)
}
