# This pair of functions is intended to calculate the inverse of a matrix and 
# assign the value to an object in a different environment.
#
#The first function, makeCacheMatrix, creates a list containg a function to:
#	1. set the value of the matrix
#	2. get the value of the matrix
#	3. set the value of the inverse matrix
#	4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x<<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<-solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# The following function calculates the inverse of the matrix created with the above
# function.  However, it checks to see if the inverse has already been calculated.
# If so, it get the inverse from the cache and skips the computation. Otherwise,
# it calculates the inverse and sets the vaslue in the cache via the setcache function.

cachesolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}	
				