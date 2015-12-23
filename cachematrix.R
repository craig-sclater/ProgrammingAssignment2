## This set of functions will save the state of a matrix and its inverse in a list vector
## so they can be called from cache when needed instead of recalculated.

## The makeCacheMatrix function accepts a matrix vector and assigns it to a list
## with functions for getting the matrix, as well as setting and getting the matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
	s <<- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) s <<- solve
	getinverse <- function() s
	list(set = set, get = get,
	     setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function will accpet a list vector and call its getsolve function
## to retrive a possibly stored matrix inverse.  If the matrix inverse exists, it will
## return it; if the matrix inverse does not exist, it will call the setsolve function
## of the list to assign the matrix inverse.
cacheSolve <- function(x, ...) {
	s <- x$getinverse()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data)
	x$setinverse(s)
	s
}
