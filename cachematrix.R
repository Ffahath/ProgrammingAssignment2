## The makeCacheMatrix() and cacheSolve() work together to enable
## caching of results (matrix inverse) instead of calculating
## the matrix inverse over and over again

## This function provides means to use a cached version
## of matrix inverse, if available.

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    
    # set the matrix and reset the cached inverse 
    set <- function(y) {
        x <<- y
        cached_inverse <<- NULL
    }
    
    # retrieve the current matrix
    get <- function() x
    
    # store the inverse into the cached variable
    setinverse <- function(inverse) cached_inverse <<- inverse
    
    # retrieve the cached inverse result
    getinverse <- function() cached_inverse
    
    # return prepared caching object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    # retrieve the cached result from the caching object
    inv <- x$getinverse()
    
    # no need to compute the inverse, if the cache result is ready
    if(!is.null(inv)) {
        message("cached data retrieved")
        return(inv)
    }
    
    data <- x$get()
    # compute the inverse matrix
    inv <- solve(data)
    # store the inverse matrix into the caching object
    x$setinverse(inv)
    inv
}

## small test to check the correct functionality
test <- function()
{
    A = matrix(c(2, 4, 3, 1), nrow=2, ncol=2) 
    identityM = matrix(c(1, 0, 0, 1), nrow=2, ncol=2) 
    
    env = makeCacheMatrix(A)
    
    invA1 = cacheSolve(env)
    print(invA1)
    invA2 = cacheSolve(env)

    if (!all.equal(A %*% invA2, identityM)) {
        message("Incorrect result!")
    }
    
    B = matrix(c(4, 2, 2, 3), nrow=2, ncol=2) 
    # switch the background matrix
    env$set(B)
    
    invB1 = cacheSolve(env)
    print(invB1)
    if (!all.equal(B %*% invB1, identityM)) {
        message("Incorrect result!")
    }
    invB2 = cacheSolve(env)
    invB2 = cacheSolve(env)
    
}