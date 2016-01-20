## When used together, these functions optimize the performance of finding the inverse of matrix by
## caching the inversion.

## This function creates the function constructors for use in caching a matrix. Variables and functions are
## referenced by point in a non-local environment using the "<<-" operator.
##
## Input: a matrix
## Output: a list of functions to set/get a matrix, to set/get an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function checks to see if the inverse matrix is already cached.  If so, it returns
## the cached inverse, otherwise it uses the solve function to find the inverse of matrix x,
## caches the result and returns the inverse.
##
## Input:  output of makeCacheMatrix
## Output:  inverse of the original matrix imput to makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
        
}

