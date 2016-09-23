library(parallel)
library(yaplr)

library(testthat)

context("Init guards")
suppressWarnings(yaplr:::shutdown_client())
yaplr:::shutdown_server()

test_that("Is client initialized", {
	expect_false(is_client_initialized())
	expect_false(is_server_initialized())
	expect_false(is_server_running())
})

test_that("Client without a server",{
	expect_error(yaplr:::init_client(),regexp = 'Cannot initialize client before initialization of the server' )
})

test_that("Multiple initialization of the server", {
	expect_false(is_server_initialized())
	expect_true(yaplr:::init_server())
	suppressWarnings(expect_false(yaplr:::init_server()))
	expect_warning(yaplr:::init_server(),regexp = 'The server seems to be already initialized here')
	expect_false(yaplr:::init_server(force = TRUE))
	expect_true(yaplr:::shutdown_server())
	expect_false(yaplr:::shutdown_server())
})

test_that("Multiple client inits",{
	fn<-function()
	{
		library(yaplr)
		yaplr:::init_server()
		Sys.sleep(2)
		yaplr:::shutdown_server()
	}
	con<-mcparallel(fn())
	Sys.sleep(0.2)
	expect_true(yaplr:::init_client())
	expect_true(is_client_initialized())
	expect_false(yaplr:::init_client())
	expect_true(yaplr:::shutdown_client())
	suppressWarnings(expect_false(yaplr:::shutdown_client()))
	expect_warning(yaplr:::shutdown_client(), regexp = 'Client was not initialized anyway')
	mccollect()
})

test_that("Multiple server inits",{
	fn<-function()
	{
		library(yaplr)
		yaplr:::init_server()
		Sys.sleep(2)
		yaplr:::shutdown_server()
	}
	con<-mcparallel(fn())
	Sys.sleep(0.2)
	expect_error(yaplr:::init_server(), regexp = 'Server was already initialized somewhere else')
	yaplr:::shutdown_server()
	mccollect()
})

test_that("Initialize client on the server",{
	suppressWarnings(yaplr:::shutdown_client())
	yaplr:::shutdown_server()

	yaplr:::init_server()
	expect_false(yaplr:::init_client(server_ok=TRUE))


	expect_warning(yaplr:::shutdown_client(), regexp = 'Shutting down client on server does nothing')
	yaplr:::shutdown_server()
})

test_that("Initialize client when shared file was removed",{
	suppressWarnings(yaplr:::shutdown_client())
	yaplr:::shutdown_server()

	yaplr:::init_server()
	shared_file<-getOption('yaplr_shared_file')
	expect_true(file.exists(shared_file))
	unlink(shared_file)

	expect_error(yaplr:::init_client(),regexp = 'The shared file is missing. Is server really running')

	expect_warning(f<-yaplr:::shutdown_client(),regexp = 'Client was not initialized anyway, no need to shut down')
	expect_false(f)
	yaplr:::shutdown_server()
})
