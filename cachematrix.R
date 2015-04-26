## Matrix inversion is usually a costly computation, the following 
## functions create a special matrix object that can cache it's inverse
## and retrieves it's inverse and/or calculates it's inverse.

## makeCacheMatix creates a special "matrix" object that can cache its
## inverse.  This special object has a pair of get/set accessors for the
## original matrix and its inverse called get, set, getinverse, and 
## setinverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

## cacheSolve returns the inverse of the special "matrix" returned by 
## makeCacheMatrix.  If the matrix has already been calcualted, then
## cacheSolve retrieves the matrix from the cache, otherwise it
## calculates the inverse using sovle() and stores the inverse in the
## cache.  cacheSolve assumes x is a square invertible matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m) ## return from the cache
	}
	data <- x$get()	      ## retrieve original matrix
	m <- solve(data, ...) ## calcuate inverse
	x$setinverse(m)	      ## store inverse in cache
	m		      ## return inverse
}
