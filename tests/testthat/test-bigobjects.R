library(parallel)
library(yaplr)
library(testthat)

context("Sending large messages")
suppressWarnings(yaplr:::shutdown_client())
yaplr:::shutdown_server()
yaplr:::reset_communication()

test_that("Sending and receivinig big object, server side",{
	m1<-yaplr:::attach_mutex('lock1')
	suppressWarnings(synchronicity::lock(m1,block=FALSE))
	bigobj<-1:16000
	bigobj_size<-object.size(bigobj)
	clientfn<-function()
	{
		library(yaplr)
		m<-yaplr:::attach_mutex('lock1')
		synchronicity::lock(m)
		Sys.sleep(0.2) #Waiting 200ms for server loop to come up is plenty
		yaplr:::init_client()
		yaplr:::send_to_server('ping',list(noop=bigobj))
		yaplr:::send_to_server('quit',NULL)
		yaplr:::shutdown_client()
	}
	yaplr:::shutdown_server()
	yaplr:::reset_communication()
	con<-mcparallel(clientfn())

	synchronicity::unlock(m1) #Allow client to send messages in 200ms.

	yaplr:::server_loop(quiet = TRUE)

	mccollect(con)
	yaplr:::shutdown_server()
})


test_that("Sending and receivinig big object, client side",{
#	debugonce(yaplr:::auto_init)
	expect_message(yaplr:::auto_init(), regexp = 'Server process spawned')
	bigobj<-1:16000
	bigobj_size<-object.size(bigobj)
#	debugonce(yaplr:::send_to_server)
	ans<-yaplr:::send_to_server('ping',list(noop=bigobj))
	expect_equal(bigobj_size, ans)
	#debugonce(yaplr::make_sure_server_is_down)
	yaplr::make_sure_server_is_down()
})

