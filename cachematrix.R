## Two functions to solve the matrix and keep result cached,
## thus subsequent calls to solve same matrix return cached result

## makeCacheMatrix - function creates a structure for keeping original and inverse
##                  matrix. Provides methods to access these matrices:
##                  $get() - get original matrix
##                  $set() - re-set matrix to solve
##                  $getinv() - read inverse matrix (does not solve)
##                  $setinv() - saves inverse matrix in cache

makeCacheMatrix <- function(x = matrix()) {

    # initialize cache
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve - solves matrix for given cached "structure", saves inverse matrix in cache
##              and returns it

cacheSolve <- function(x, ...) {
    
    # check cache
    inv <- x$getinv()
    if(!is.null(inv)) {
        
        # return from cache
        message("getting cached data")
        return(inv)
    }
    
    # not yet cached, solve
    # get matrix to solve
    data <- x$get()
    # solve
    inv <- solve(data, ...)
    # save inverse to cache
    x$setinv(inv)
    
    # return inverse
    inv
}


# testing
#m <- matrix(runif(16),4,4)
#mc<-makeCacheMatrix(m)
#class(mc)
#cacheSolve(mc)
#mc$get()
#mc$getinv()
