## Define functions that create a matrix and its inverse matrix,
## and enable caching functionality to optimize calculation time.
## Using global assignment or superassignment

## Create and cache (varible assignment in extended scope) a matrix 
## and its inverse matrix, upon calling sub-functions from cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
	
}


## Calculate inverse matrix and keep in cache unless existing,
## always return (either calculated or cached) inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
