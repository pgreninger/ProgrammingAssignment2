################################################################################
## This file contains defintions for a pair of functions designed to solve for
## and cache the inverse of a matrix. It assumes the matrix is invertible.
## Basic usage:
##
##    myMatrix<-matrix(c(2,2,3,2), nrow=2)
##    cacheMatrix<-makeCacheMatrix(myMatrix)
##    inverseMatrix<-cacheSolve(cacheMatrix)
##
################################################################################

## This function takes an optional invertible matrix argument, or creates an
## empty matrix by default. Accessor functions allow getting and setting the
## unerlying matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list( set = set, get = get, setinverse = setinverse,
          getinverse = getinverse)
}


## This function checks whether a matrix inverse has already been calculated
## and either returns a previously calculated inverse or calculates, caches
## and returns the matrix inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

