# Yet Another Parallel Library for R

This package significantly lowers the difficulty for efficient sending R objects between different R sessions on the same machine. 

Objects are shared via an intermediary server R process, that is lazily spawned, and listens for user requests. 
All communication is done via shared memory (package bigmemeory) and mutexes (package synchronicity).

The whole process is designed with efficiency in mind, for both small and large objects. 

## The workflow

The process should be pretty straight-forward. R session number 1:

```{r}
library(yaplr)
send_object(obj=1:10, tag='myobject')
# Server process spawned
```

R session number 2:
```{r}
library(yaplr)
list_objects()
#          size                    ctime
# myobject   62 Sat Sep 24 13:01:57 2016
retrieve_object(tag='myobject')
# [1]  1  2  3  4  5  6  7  8  9 10
remove_object('myobject')
quit_server()
```

Each session is treated equaly. There can be as many sessions as you would like. 
You can even access the objects after the session that originated the object has ended. 
To free up the memory taken by single variable use `remove_object`. 

`quit_server` quits the server, freeing all memory used for stored variables.

## Memory footprint

Server is implemented in R. It uses very little memory in itself - 6 mutexes, pointer to the `big.matrix` and an environment for stored objects. 
Each object is stored as a unserialized `bigmemory::big.matrix` of type `'raw'` together with single metadata `ctime` - creation time.

Since the server is spawned in separate process, the amount of memory it occuppies is not reported to other R sessions. 
Similarily, the memory taken by the stored objects is not visible to R. 

Because the variables are internally stored as shared memeory, 
it is visible in `htop` as "shared" memory (as opposed to "used" memory), which might be confusing for some people. 

## Troubleshooting

The package relies heavily on mutexes. Because of [bug in the `synchronicity` package](https://github.com/kaneplusplus/synchronicity/issues/7) that prevents from using timeout feature,
many bugs in `yaplr` will manifest itself as deadlocks. In such case, the best way to recover is running the following commands on another R session:
`yaplr:::reset_communications()` or `yaplr:::init_server()` followed by `yaplr:::shutdown_server()`. You will loose the shared objects, with possible memory leak, but the locked R session will hopefully 
unlock and you will be able to recover your work, and possibly continue using this package.

## Known issues & feature requests

See the GitHub's Issues tab to see all unresolved issues. 

You can post feature requests there as well, and hopefully I will have resources to address them all.

