library(parallel)
library(yaplr)
library(testthat)

context("Simple localhost communication")
suppressWarnings(yaplr:::shutdown_client())
yaplr:::shutdown_server()

test_that("Test is_server_running", {
	m1<-yaplr:::attach_mutex('lock1')
	suppressWarnings(synchronicity::lock(m1,block=FALSE))
	fn<-function()
	{
		library(yaplr)
		m<-yaplr:::attach_mutex('lock1')
		yaplr:::init_server()
		synchronicity::unlock(m)
		yaplr:::server_loop(quiet = TRUE)
		yaplr:::shutdown_server()
	}
	con<-mcparallel(fn())
	synchronicity::lock(m1) #Waiting for server start

	expect_true(is_server_running())

	yaplr:::init_client()
#	debugonce(send_to_server)
	expect_equal(yaplr:::send_to_server(method='ping', args = list()),'pong')

	expect_null(yaplr:::send_to_server(method = 'quit',args = list()))
	yaplr:::shutdown_client()
	mccollect(con)
})

test_that("Test ping-pong, server side", {
	m1<-yaplr:::attach_mutex('lock1')
	suppressWarnings(synchronicity::lock(m1,block=FALSE))
	clientfn<-function()
	{
		Sys.sleep(1)
		library(yaplr)
		m<-yaplr:::attach_mutex('lock1')
		synchronicity::lock(m) #Waiting for server start
		yaplr:::init_client()

		Sys.sleep(0.2) #Waiting for the server loop to come up in the old-fassioned way. 200ms is plenty for it.
		yaplr:::send_to_server(method='ping', args = list())
		yaplr:::send_to_server('quit',NULL)
		yaplr:::shutdown_client()
	}
	yaplr:::shutdown_server()
	yaplr:::reset_communication()
	con<-mcparallel(clientfn())
	yaplr:::init_server()

	synchronicity::unlock(m1) #Allowing client start


#	debugonce(server_loop)
	yaplr:::server_loop(quiet = TRUE)

	mccollect(con)
	yaplr:::shutdown_server()
})

test_that("Test ping-pong, client side", {
	m1<-yaplr:::attach_mutex('lock1')
	suppressWarnings(synchronicity::lock(m1,block=FALSE))

	serverfn<-function()
	{
		library(yaplr)
		yaplr:::init_server()
		m<-yaplr:::attach_mutex('lock1')
		synchronicity::unlock(m) #Signalling that we are just about entering the event loop

		yaplr:::server_loop(quiet = TRUE)
		yaplr:::shutdown_server()
	}


	con<-mcparallel(serverfn())

	synchronicity::lock(m1) #Wait for server to come up
	Sys.sleep(0.1) #Wait a little bit more for the event loop
	yaplr:::init_client()
	yaplr:::send_to_server('quit',NULL)

	mccollect(con)
	yaplr:::shutdown_client()
})
