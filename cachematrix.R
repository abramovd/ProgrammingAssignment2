# Matrix inversion is usually a costly computation and there may
# be some benefit to caching the inverse of a matrix rather than
# compute it repeatedly. These two functions cache are used to cache the inverse 
# of a matrix.

# makeCacheMatrix is used to create a special object that stores
# a matrix. This object has such functions:
# 1. set - set the value of the matrix
# 2. get - get the value of the matrix
# 3. setinv - set the value of the inverse of the matrix
# 4. getinv - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialization
    inv <- NULL
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse of the matrix
    setinv <- function(invers) inv <<- invers
    
    # set the value of the inverse of the matrix
    getinv <- function() inv
    
    # return the list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve is used to calucalate the inverse of a matrix,
## but firstly if the inverse has already been calculated
## (and the matrix has not changed) and if it's so it retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    # checking if the inverse of the matrix have been computed
    inv <- x$getinv()
    if (!is.null(inv)) {
        # if so, get the result from the cache
        message("getting cached data")
        return(inv)
    }
    # if no get the matrix 
    data <- x$get()
    # compute the inverse
    inv <- solve(data)
    # set the value of inverse
    x$setinv(inv)
    # return the final result
    inv
}
