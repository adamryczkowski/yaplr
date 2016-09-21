library(parallel)
library(yaplr)
library(testthat)

context("Simple localhost communication")
suppressWarnings(shutdown_client())
shutdown_server()

test_that("Sending and receivinig objects, client side",{

})

test_that("Sending and receivinig objects, server side",{
	clientfn<-function()
	{
		Sys.sleep(1)
		library(yaplr)
		init_client()
		send_to_server(method='ping', args = list())
		send_to_server('quit',NULL)
		shutdown_client()
	}
	shutdown_server()
	reset_communication()
	con<-mcparallel(clientfn())
	#	init_server(force=TRUE)

	#	debugonce(server_loop)
	server_loop()

	mccollect(con)
	shutdown_server()

})
