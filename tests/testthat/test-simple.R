library(parallel)
library(yaplr)
library(testthat)

context("Simple localhost communication")
suppressWarnings(shutdown_client())
shutdown_server()

test_that("Test is_server_running", {
	fn<-function()
	{
		library(yaplr)
		init_server()
		server_loop()
		shutdown_server()
	}
	con<-mcparallel(fn())
	Sys.sleep(0.2)

	init_client()
#	debugonce(send_to_server)
	expect_equal(send_to_server(method='ping', args = list()),'pong')

	expect_null(send_to_server(method = 'quit',args = list()))
	shutdown_client()
	mccollect(con)
})

test_that("Test ping-pong, server side", {
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

test_that("Test ping-pong, client side", {
	serverfn<-function()
	{
		library(yaplr)
		init_server()
		server_loop()
		shutdown_server()
	}


	con<-mcparallel(serverfn())

	Sys.sleep(0.1)
	init_client()
	send_to_server('quit',NULL)

	mccollect(con)
	shutdown_client()
})
