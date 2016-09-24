library(parallel)
library(yaplr)
library(testthat)

context("Handling objects")
suppressWarnings(yaplr:::shutdown_client())
yaplr:::shutdown_server()
yaplr:::reset_communication()

test_that("Sending an object, server side",{
	m1<-yaplr:::attach_mutex('lock1')
	m2<-yaplr:::attach_mutex('lock2')
	suppressWarnings(synchronicity::lock(m1,block=FALSE))
	suppressWarnings(synchronicity::lock(m2,block=FALSE))

	obj<-runif(100)
	clientfn1<-function()
	{
		library(yaplr)
		m<-yaplr:::attach_mutex('lock1')
		synchronicity::lock(m) #Waiting server to come up

		yaplr:::init_client()
		m<-yaplr:::attach_mutex('lock2')
		synchronicity::unlock(m) #Signalling start of sending stuff in 200ms
		Sys.sleep(0.2)

		yaplr::send_object(obj = obj, tag =  'myobject')
		yaplr::list_objects()
		yaplr::does_object_exist('xxx')
		yaplr::does_object_exist('myobject')
		yaplr::send_object(obj = obj, tag =  'myobject2')
		yaplr::remove_object(tag='myobject2')
		yaplr:::send_to_server('quit',NULL)
		yaplr:::shutdown_client()
	}
	yaplr:::shutdown_server()
	yaplr:::reset_communication()
	# debugonce(yaplr:::remotecall_list_objects)
	# debugonce(yaplr:::remotecall_store_object)
	con<-parallel::mcparallel(clientfn1())

	yaplr:::init_server()
	synchronicity::unlock(m1) #Allow client to initialize
	synchronicity::lock(m2) #Wait until client finished initialization
#	debugonce(yaplr:::remotecall_list_objects)
#
	yaplr:::server_loop(quiet = TRUE)

	mccollect(con)

	expect_equal(ls(name=.object_storage),'myobject')
#	debugonce(yaplr:::remotecall_retrieve_object)
	yaplr:::remotecall_retrieve_object(tag='myobject')
	obj2<-unserialize(connection =  .object_storage$myobject$obj[,1])
	expect_equal(obj,obj2)

	yaplr:::shutdown_server()
})

test_that("Sending an object, client side",{
	m1<-yaplr:::attach_mutex('lock1')
	m2<-yaplr:::attach_mutex('lock2')
	suppressWarnings(synchronicity::lock(m1,block=FALSE))
	suppressWarnings(synchronicity::lock(m2,block=FALSE))

	obj<-runif(100)
	clientfn1<-function()
	{
		Sys.sleep(1)
		library(yaplr)
		m<-yaplr:::attach_mutex('lock1')
		synchronicity::lock(m) #Waiting server to come up

		yaplr:::init_client()

		m<-yaplr:::attach_mutex('lock2')
		synchronicity::unlock(m) #Signalling start of sending stuff in 200ms
		Sys.sleep(0.2)

		yaplr::send_object(obj = obj, tag =  'myobject')
		yaplr::retrieve_object(tag='myobject')
		yaplr::quit_server()
	}

	yaplr:::shutdown_server()
	yaplr:::reset_communication()



	con<-mcparallel(clientfn1())

#	debugonce(yaplr:::server_loop)
	yaplr:::init_server()
	synchronicity::unlock(m1) #Allow client to initialize
	synchronicity::lock(m2) #Wait until client finished initialization
	yaplr:::server_loop(quiet = TRUE)

	mccollect(con)

	yaplr:::shutdown_server()
})

test_that("Sending and receiving an object, client side",{

	obj<-runif(100)
	send_object(obj = obj, tag = 'myobj2')

	obj2<-retrieve_object(tag='myobj2')
	expect_equal(obj,obj2)

	yaplr::quit_server()

})

test_that("The ultime test: sending in one thread, and receiving in another",{
	obj<-runif(100)

	clientfn<-function()
	{
		Sys.sleep(1)
		library(yaplr)
		send_object(obj=obj, tag='myobject')
	}

	yaplr:::shutdown_server()
	yaplr:::reset_communication()
	con<-mcparallel(clientfn())
	mccollect(con)

	obj2<-retrieve_object(tag='myobject')


	expect_equal(obj,obj2)
#	debugonce(list_objects)
	ans<-list_objects()

	ans<-does_object_exist(tag='myobject')
	expect_true(ans)
	remove_object(tag='myobject')
	ans<-does_object_exist(tag='myobject')
	expect_false(ans)

	yaplr::quit_server()

})

