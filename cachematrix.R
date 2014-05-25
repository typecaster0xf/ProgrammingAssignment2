## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(internalMatrix = matrix())
{
	inverse <- NULL
	
	set <- function(newMatrix)
	{
		internalMatrix <<- newMatrix
		inverse <- NULL
	}
	
	get <- function()
		internalMatrix
	
	setInverse <- function(newInverse)
		inverse <<- newInverse
	
	getInverse <- function()
		inverse
	
	#----
	
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(cacheMatrix, ...)
{
	inverseOfCacheMatrix <- cacheMatrix$getInverse()
	
	if(is.null(inverseOfCacheMatrix))
	{
		inverseOfCacheMatrix <- solve(cacheMatrix$get(), ...)
		cacheMatrix$setInverse(inverseOfCacheMatrix)
	}
	
	inverseOfCacheMatrix
}
