# This file provides a type that wraps around a
# matrix.  It can be used with the "cacheSolve"
# function to cache the inverse of the contained
# matrix within the cacheMatrix list, (at least
# that's what I would say if this was really
# object oriented; it appears that it is really
# a clojure, storing the matrix and its inverse
# in the environment of the "makeCacheMatrix"
# function call that created it, which doesn't
# really exist at the time when you use it, but
# since there are still active references to the
# variables it contained, they are just sort of
# hanging in memory somewhere).
# 
# This allows you to not worry about making
# extraneous calls to solve() for all your matrix
# inversion needs by using "cacheSolve" instead,
# which checks if the inverse has already been
# calculated and, if so, returns the cached
# result without running solve() an additional
# time.  Thus, you don't have to worry about
# culling these extraneous calls to solve() at a
# higher level.
# 
# This was done to satisfy the requirements of
# Programming Assignment 2 in the R Programming
# class on Corsera.org offered by Johns Hopkins
# University.
#################################################


# Provide this function with a matrix and it will
# return a list.  This list has two functions
# that you should be aware of: get() and set().
# Both act in regard to the matrix that you
# initialized when you called "makeCacheMatrix"
# where "get" will retreive this matrix and "set"
# will overwrite that matrix with the new one
# that you supply to "set."  Note that calls to
# "set" will clear the cache of the matrix'
# inverse.
# 
# (There are also two functions that
# deal with the cached inverse of the internal
# matrix, but you should not try to use these
# functions directly, as there is the potential
# to destabalize your program by writing a bad
# matrix in as the inverse.  Use "cacheSolve"
# instead.)
# 
# The one time I would advocate calling
# "setInverse" is iff you pass it NULL to force
# clearing of the cache in the case where you'
# need to rerun "cacheSolve" and pass different
# arguments in to the internal call to solve(),
# as the call to solve() will only be made if the
# cache is clear, otherwise the cached solution
# will be returned without any thought given to
# the effect that it might somehow no longer be
# the correct answer.
makeCacheMatrix <- function(internalMatrix = matrix())
{
	# This variable, along with the function
	# argument, will persists as a clojure, it is
	# here initialized to NULL so that if this
	# paticular matrix never needed to be
	# inverted, then we aren't wasting a call to
	# solve() here.
	inverse <- NULL
	
	# Overwrites the matrix and clears the cached
	# inverse.
	set <- function(newMatrix)
	{
		internalMatrix <<- newMatrix
		inverse <- NULL
	}
	
	# Returns the actual matrix.
	get <- function()
		internalMatrix
	
	# Updates the cached value for the inverse of
	# the matrix.  DO NOT CALL THIS FUNCTION
	# TO DO ANYTHING OTHER THAN FORCE CLEAR THE
	# CACHED INVERSE, UNLESS YOU KNOW WHAT YOU
	# ARE DOING!  YOU HAVE BEEN WARNED!
	setInverse <- function(newInverse)
		inverse <<- newInverse
	
	# Returns the cached value of the matrix'
	# inverse.  If the inverse has not been
	# calculated then NULL will be returned, so
	# YOU SHOULD NOT CALL THIS FUNCTION DIRECTLY!
	# USE "cacheSolve" INSTEAD!
	getInverse <- function()
		inverse
	
	#----
	
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}# makeCacheMatrix(internalMatrix)

# Call this function whenever you need to get the
# inverse of the matrix!  (For that matter, any
# time you need to make a call to solve() where
# the matrix contained in the cacheMatrix list is
# the first argument and you want the solution to
# be stored in the cache.)
# 
# This function requires a list of the type
# returned from "makeCacheMatrix".  Additional
# arguments will be passed on to the call to
# solve().
# 
# If the cacheMatrix already has a value cached
# for the call to solve() then it will be
# returned without calling solve().
# 
# Please note that this function NEVER overwrites
# a cached solution, so if this function is
# called with different arguments to be passed to
# solve, then the cached solution will be
# returned even though it is probably incorrect
# in this circumstance!  Since this is not
# expected to be the common case (by a long
# shot), no code to track differing arguments
# between calls to "cacheSolve" has been written,
# but this is still something to keep in mind.
# Should such a need ever arise, call
# "setInverse(NULL)" on the cacheMatrix to force
# the cache to be cleared, with will cause
# "cacheSolve" to accept the new arguments and
# pass them on to solve().
cacheSolve <- function(cacheMatrix, ...)
{
	inverseOfCacheMatrix <- cacheMatrix$getInverse()
	
	#----
	
	# If the value that has been pulled out of
	# the cacheMatrix' cache of its inverse is
	# NULL, then calculate and store the inverse.
	if(is.null(inverseOfCacheMatrix))
	{
		inverseOfCacheMatrix <- solve(cacheMatrix$get(), ...)
		cacheMatrix$setInverse(inverseOfCacheMatrix)
	}
	
	# Even if the call to "getInverse" originally
	# returned NULL, the above if-statement
	# ensures that the inverse is now stored in
	# this local variable, from where it is now
	# being returned.
	inverseOfCacheMatrix
}#cacheSolve(cacheMatrix, ...)

############################
# EXAMPLE USE OF THIS FILE #
############################
# 
# > source("cachematrix.R")
# > cm <- makeCacheMatrix(matrix(c(1,4,5,3,1,2,3,2,2,2,3,4,5,3,2,1), ncol=4, nrow=4))
# > cm$getInverse()
# NULL
# > cacheSolve(cm)
#             [,1]       [,2]        [,3]        [,4]
# [1,] -0.43478261  1.1739130 -0.78260870  0.21739130
# [2,]  0.65217391 -2.2608696  2.17391304 -0.82608696
# [3,] -0.04347826  0.2173913 -0.47826087  0.52173913
# [4,]  0.17391304  0.1304348 -0.08695652 -0.08695652
# > cm$get()
#      [,1] [,2] [,3] [,4]
# [1,]    1    1    2    5
# [2,]    4    2    2    3
# [3,]    5    3    3    2
# [4,]    3    2    4    1
# > cm$getInverse()
#             [,1]       [,2]        [,3]        [,4]
# [1,] -0.43478261  1.1739130 -0.78260870  0.21739130
# [2,]  0.65217391 -2.2608696  2.17391304 -0.82608696
# [3,] -0.04347826  0.2173913 -0.47826087  0.52173913
# [4,]  0.17391304  0.1304348 -0.08695652 -0.08695652
# > q()
