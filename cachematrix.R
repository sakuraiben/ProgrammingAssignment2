## Put comments here that give an overall description of what your
## functions do
##	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##        cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
	inverse<-NULL
	
	
	set <- function(y) { 
		x <<- y 
		inverse <<- NULL
	}
	
	get <- function() { x }
	
	setInverse <- function(inv) { inverse <<- inv}
	
	getInverse <- function() { inverse }
	
	
	
	
	list(
	set = set,
	get = get,
	setInverse = setInverse,
	getInverse = getInverse
	)
	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        if(!is.null(x)) { 
        	print("retrieving cached data ")
        	return(x)
        }
        
        data <- x$get()
        x <- solve(inverse)
        x$setInverse(inverse)
        inverse
}
