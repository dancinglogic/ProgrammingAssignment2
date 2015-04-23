##  This file contains a pair of functions that cache the inverse of a matrix.
##    
##  makeCacheMatrix: creates a special "matrix" object that can 
##                   cache its own inverse.
##  cacheSolve:      Computes the inverse of the special "matrix" returned by 
##                   makeCacheMatrix above. If the inverse has already been 
##                   calculated (and the matrix has not changed), then 
##                   cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix is a function that returns a list of 4 functions:
##     set, get, setinverse, getinverse
## The point of these functions is to store a matrix and its inverse. 
##
## The 'set' and 'get' functions let you set the value of the matrix, and
## retrieve the value of the matrix.
## The 'setinverse' and 'getinverse' functions let you set and retrieve
## the value of the inverse of the matrix.
##
## Note: initially the value of the inverse is set to NULL. The inverse must 
## be specified using 'setinverse' before 'getinverse' will be any use.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
            x <<- y
            # Since we're resetting the matrix, better reset the inverse too.
            inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    # Return a list containing the 4 functions we just made
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## Given a matrix (as created by makeCacheMatrix above), return its inverse.
## We may assume the matrix is invertible.

cacheSolve <- function(x, ...) {
    # Get the inverse if it has been computed (if it hasn't, will get NULL).
    inverse <- x$getinverse()
    # If the inverse hasn't been computed yet, compute it and store it.
    if(is.null(inverse)){
        m <- x$get()
        inverse <- solve(m)
        x$setinverse(inverse)
    }
    ## Return a matrix  that is the inverse of 'x'
    inverse
}
