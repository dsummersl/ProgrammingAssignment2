## These functions provide a wrapper for a matrix that caches its inverse.
##
## Example usage:
##
## m <- makeCacheMatrix(matrix(data=c(1,0,0,1),2,2))
## # compute inverse
## cacheSolve(m)
## # second call will not compute another inverse
## cacheSolve(m)
## # return cached value
## m$inverse()
## # return cached value
## m$inverse()
##

## Creates a special 'matrix' capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrixValue <- x
    inverse <- NULL
    setmatrix <- function(m) {
        matrixValue <<- m
        inverse <<- NULL
    }
    getmatrix <- function() matrixValue
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(
        set = setmatrix,
        get = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Find the inverse of the matrix contained in x, cache it, and return it.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        return(i)
    }
    m <- x$get()
    i <- solve(m)
    x$setinverse(i)
    i
}
