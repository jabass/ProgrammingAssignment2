## The "makeCacheMatrix" function can be used to
## wrap a matrix and its inverse.  The "cacheSolve"
## function will return the inverse of a matrix,
## computing and caching it on the first call, and 
## retrieving it from a cache on subsequent calls.

## makeCacheMatrix wraps an input matrix, providing
## get/set accessors.  The function can also store 
## the inverse of the matrix, also providing
## get/set accessors.
## Input matrix must be invertible.
makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) invX <<- solve
    getInverse <- function() invX
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve returns the inverse of the input
## matrix "x".  Input matrix "x" must have been 
## constructed using the "makeCacheMatrix" function.
## The first time it is called with
## a particular matrix, the inverse is computed, stored
## in the cache and returned.  Subsequent calls with
## the same matrix as input will return the inverse from the cache.
cacheSolve <- function(x, ...) {
    invX <- x$getInverse()
    if(!is.null(invX)) {
        message("getting cached inverse")
        return(invX)
    }
    data <- x$get()
    invX <- solve(data, ...)
    x$setInverse(invX)
    invX
}
