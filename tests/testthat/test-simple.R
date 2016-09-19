library(parallel)
library(yaplr)

context("Simple localhost communication")

test_that("Server startup and teardown", {
	shutdown_server()
	init_server()
	shutdown_server()
})

test_that("Client startup and teardown", {
	client_shutdown()
	init_client()
	client_shutdown()
})

test_that("Test ping-pong, server side", {
	clientfn<-function()
	{
		Sys.sleep(0.1)
		library(yaplr)
		init_client()
		reset_mutexes()
		send_to_server('quit',NULL)
		shutdown_client()
	}

	init_server()

	con<-mcparallel(clientfn())

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
