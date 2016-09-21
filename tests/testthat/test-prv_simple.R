library(parallel)
library(yaplr)
library(testthat)

context("Simple localhost communication")
suppressWarnings(yaplr:::shutdown_client())
yaplr:::shutdown_server()

test_that("Test is_server_running", {
	fn<-function()
	{
		library(yaplr)
		yaplr:::init_server()
		yaplr:::server_loop()
		yaplr:::shutdown_server()
	}
	con<-mcparallel(fn())
	Sys.sleep(0.2)

	expect_true(is_server_running())

	yaplr:::init_client()
#	debugonce(send_to_server)
	expect_equal(yaplr:::send_to_server(method='ping', args = list()),'pong')

	expect_null(yaplr:::send_to_server(method = 'quit',args = list()))
	yaplr:::shutdown_client()
	mccollect(con)
})

test_that("Test ping-pong, server side", {
	clientfn<-function()
	{
		Sys.sleep(1)
		library(yaplr)
		yaplr:::init_client()
		yaplr:::send_to_server(method='ping', args = list())
		yaplr:::send_to_server('quit',NULL)
		yaplr:::shutdown_client()
	}
	yaplr:::shutdown_server()
	yaplr:::reset_communication()
	con<-mcparallel(clientfn())
#	init_server(force=TRUE)

#	debugonce(server_loop)
	yaplr:::server_loop()

	mccollect(con)
	yaplr:::shutdown_server()
})

test_that("Test ping-pong, client side", {
	serverfn<-function()
	{
		library(yaplr)
		yaplr:::init_server()
		yaplr:::server_loop()
		yaplr:::shutdown_server()
	}


	con<-mcparallel(serverfn())

	Sys.sleep(0.1)
	yaplr:::init_client()
	yaplr:::send_to_server('quit',NULL)

	mccollect(con)
	yaplr:::shutdown_client()
})
