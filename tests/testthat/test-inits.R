library(parallel)
library(yaplr)

library(testthat)

context("Init guards")
suppressWarnings(shutdown_client())
shutdown_server()

test_that("Client without a server",{
	expect_error(init_client(),regexp = 'Cannot initialize client before initialization of the server' )
})

test_that("Multiple initialization of the server", {
	expect_true(init_server())
	suppressWarnings(expect_false(init_server()))
	expect_warning(init_server(),regexp = 'The server seems to be already initialized here')
	expect_false(init_server(force = TRUE))
	expect_true(shutdown_server())
	expect_false(shutdown_server())
})

test_that("Multiple client inits",{
	fn<-function()
	{
		library(yaplr)
		init_server()
		Sys.sleep(2)
		shutdown_server()
	}
	con<-mcparallel(fn())
	Sys.sleep(0.2)
	expect_true(init_client())
	expect_false(init_client())
	expect_true(shutdown_client())
	suppressWarnings(expect_false(shutdown_client()))
	expect_warning(shutdown_client(), regexp = 'Client was not initialized anyway')
	mccollect()
})

test_that("Multiple server inits",{
	fn<-function()
	{
		library(yaplr)
		init_server()
		Sys.sleep(2)
		shutdown_server()
	}
	con<-mcparallel(fn())
	Sys.sleep(0.2)
	expect_error(init_server(), regexp = 'Server was already initialized somewhere else')
	shutdown_server()
	mccollect()
})

