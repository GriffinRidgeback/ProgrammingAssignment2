## Solution for Programming Assignment 2 
## of the Coursera R Programming course


## This function accepts a matrix as input
## (or uses a default matrix if no argument is supplied)
##
## It creates a list of functions and returns that list
## to the caller, in a fashion similar to C function pointers
##
makeCacheMatrix <- function(x = matrix()) {
    
    # guard condition to check if x is a matrix
    if (!is.matrix(x)) {
        message("Please supply a valid matrix")
        return(list())
    }
    
    # initialize the cache matrix
    mx <- NULL
    
    # set a new data matrix and clear the cache
    set <- function(y) {
        x <<- y
        mx <<- NULL
    }
    
    # return the data matrix
    get <- function() x
    
    # set the value of an inverse matrix to the cache
    setmean <- function(mean) mx <<- mean
    
    # return the cached matrix inverse
    getmean <- function() mx
    
    # return a list of pointers to functions
    list(setMatrix = set, 
         getMatrix = get,
         setMatrixCache = setmean,
         getMatrixCache = getmean)
}

## This function accepts a matrix as input, along with
## an variable argument list of additional data to be added
## to the parameters to the solve() function
##
## It returns the inverse of the matrix supplied 
## in the argument "x"
##
cacheSolve <- function(x, ...) {

    # get the (possibly cached) matrix
    mx <- x$getMatrixCache()
    
    # no need to recalculate, use cache instead
    if(!is.null(mx)) {
        message("getting cached data")
        return(mx)
    }
    
    # no cached matrix, get data and compute
    data <- x$getMatrix()
    
    mx <- solve(data, ...)

    # store results in the cache
    x$setMatrixCache(mx)
    
    # return inverse to caller
    mx
}