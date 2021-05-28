## These functions provide a way to cache the inverse of a matrix so it
## only needs to be computed once for each time the value of the matrix
## is set.

## Creates a special matrix object that has methods for setting, getting,
## setting the inverse, and getting the inverse.

makeCacheMatrix <- function(x = matrix()) {
    mat <- x
    inv <- NULL
    set <- function(x) {
        mat <<- x
        inv <<- NULL
    }
    get <- function() mat
    setinv <- function(new_inv) inv <<- new_inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Take a special matrix object as argument, and return its inverse. If
## the inverse has already been computed since the matrix was last modified,
## return the cached value for the inverse. Otherwise compute it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
