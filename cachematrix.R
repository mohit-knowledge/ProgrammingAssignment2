## The following two functions compute the inverse of a matrix if not
## done already and cache the inverse for quick retrieval when required.

## This function creates a special "matrix" object that can cache 
## its inverse. It has get and set functions for the matrix and
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    setMatrix <- function(y) {
        x <<- y;
        inverse <<- NULL;
    }
    
    getMatrix <- function()
        return(x);
    
    setInverse <- function(inv)
        inverse <<- inv;
    
    getInverse <- function()
        return(inverse);
    
    # return the list of functions
    return(list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse
    ))
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## if inverse is already available in cache, return inverse from cache
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }

    # If not in cache, Compute inverse assuming it's a square matrix
    data <- x$getMatrix()
    inverse <- solve(data, ...)

    # Set inverse in cache for future use
    x$setInverse(inverse)
    return(inverse)
}
