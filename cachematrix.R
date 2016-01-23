## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverted <- NULL
	set <- function(y)
	{
	x <<- y
	inverted <<- NULL
	}
	get <- function() x
	setinverted <- function(i) inverted <<- i
	getinverted <- function() inverted
	list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
	i <- x$getinverted()
	if(!is.null(i)){
	message("getting cached data")
	return(i)
	}
	 
	i <- solve(x$get())
	x$setinverted(i)
	i
}
 