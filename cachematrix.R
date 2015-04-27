## Implement a function can process an inverse of a matrix
## and can cache the result.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) m <<- inv
	getInverse <- function() m
	list(set = set, get = get, 
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## Get the inverse matrix if it have been calculate,
## otherwise caculate it.
cacheSolve <- function(x, ...) {
	invm <- x$getInverse()
	if(!is.null(invm)){
		message("getting cached data")
		return(invm)
	}
	input <- x$get()
	invm<- solve(input)
	x$setInverse(invm)
}
