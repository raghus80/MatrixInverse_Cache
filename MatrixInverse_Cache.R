makeCacheMatrix <- function(x = matrix()) {
	mi <- NULL
	set <- function(y) {
		x <<- y
		mi <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) mi <<- inverse
	getinverse <- function() mi
	list(set = set, get = get, setinverse = setinverse, 
			getinverse = getinverse)
	x
}

cacheSolve <- function(x) {
	mi <- x$getinverse()
	if(!is.null(mi)) {
		message("getting cached data")
		return(mi)
	}
	data <- x$get()
	mi <- solve(data)
	x$setinverse(mi)
	mi
}