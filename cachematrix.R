## The functions makeCacheMatrix() and cacheSolve() are intended to implement
## cache operations in the R language on a matrix and its inverse

## Function makeCacheMatrix() accepts a matrix as input and returns
## a special vector to perform cache operations on the matrix and
## its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    cacheMatrix <- NULL
    
    set <- function(inputMatrix) {
        x <<- inputMarix
        cacheMatrix <<- NULL
    }
    get <- function () {
        x
    }
    setInverse <- function(inverse) {
        cacheMatrix <<- inverse
    }
    getInverse <- function() {
        cacheMatrix
    }
    
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


## Function cacheSolve() accepts as input the special vector created
## by makeCacheMatrix(). It returns the inverse of the matrix if already
## cached, otherwise, it caches the inverse of the matrix and returns
## the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Check if inverse is cached. getInverse() will return NULL if NOT cached
    cacheMatrix <- x$getInverse() 
    if (!is.null(cacheMatrix)) {
        message("Getting cached data")
        return(cacheMatrix)
    }
    
    # If execution reaches here, the inverse was not cached.
    # So, compute the inverse and cache it
    data <- x$get()
    cacheMatrix <- solve(data)
    x$setInverse(cacheMatrix)
    cacheMatrix
    
}
