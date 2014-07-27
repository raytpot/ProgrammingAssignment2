## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function expects a matrix as an argument and returns a list of functions to set/get/setinverse/getinverse 
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list (get = get, set = set, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function expects the list returned by makeCacheMatrix function as an argument and returns the inverse of the matrix. If the inverse has been computed and is cached, the cached inverse will be returned.
## Assumes that the matrix is always invertible
cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
			message("getting cached data")
			return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
