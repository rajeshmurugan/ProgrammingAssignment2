## These functions are used to make special "matrix" object and caching the inverse of a matrix

##  Creates a special "matrix" object and provides functionalities to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## To set inverse matrix
  setInverse <- function(matrix) m <<- matrix
  ## To get inverse matrix
  getInverse <- function() m
  ## List to get/set functions for inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix". If the inverse is already calculated for same data,
## then it should retrieve the inverse from the cache.
## otherwise the inverse should be calculated newly and store in cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
	## Check inverse matrix is in cache?
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
	## Compute Inverse of matrix
    m <- solve(data, ...)
    x$setInverse(m)
    m  ## Return a matrix that is the inverse of 'x'
}
