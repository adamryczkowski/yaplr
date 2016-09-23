library(parallel)
library(yaplr)
library(testthat)

context("Sending large messages")
suppressWarnings(yaplr:::shutdown_client())
yaplr:::shutdown_server()
yaplr:::reset_communication()

test_that("Sending and receivinig big object, server side",{
	bigobj<-1:16000
	bigobj_size<-object.size(bigobj)
	clientfn<-function()
	{
		Sys.sleep(1)
		library(yaplr)
		yaplr:::init_client()
		yaplr:::send_to_server('ping',list(noop=bigobj))
		yaplr:::send_to_server('quit',NULL)
		yaplr:::shutdown_client()
	}
	yaplr:::shutdown_server()
	yaplr:::reset_communication()
	con<-mcparallel(clientfn())

	yaplr:::server_loop(quiet = TRUE)

	mccollect(con)
	yaplr:::shutdown_server()
})


test_that("Sending and receivinig big object, client side",{
#	debugonce(yaplr:::auto_init)
	yaplr:::auto_init()
	bigobj<-1:16000
	bigobj_size<-object.size(bigobj)
#	debugonce(yaplr:::send_to_server)
	ans<-yaplr:::send_to_server('ping',list(noop=bigobj))
	expect_equal(bigobj_size, ans)
	yaplr::make_sure_server_is_down()
})

