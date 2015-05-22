## Coursera: R Programming
## Programming Assignment 2
## Jonathan Labin
## 2015-05-22

## This file defines functions for working with matrices and their inverses.
## One first uses makeCacheMatrix to create an object which includes functions
## for storing a matrix and its inverse.
## Then, one can use cacheSolve to obtain the inverse of the embedded matrix
## which returns a chached value if already computed.

## Creates a new special "matrix" which contains functions for storing
## and retrieving a matrix and a cached copy of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse of matrix argument x.
## If the inverse was previously computed, a cached copy is returned.
## The argument x is a matrix and is assumed to be invertable.
## The ... argument is passed directly to solve.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("returning cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
