## Demonstrate R function is able to cache potentially time-consuming computations like matrix inverse.
## There are two functions a) to create an object which holds the cache attribute and functions to manipulate
## this cache attribute and raw matrix and b) to check if inverse is already available in cache or not 
## and calculate it if it is not avialable in the cache and cache it for future consumption 

## This method create the object that will hold the cache and getters and setter method to set the inverse matrix
## in the cache attribute. Note that this method does not actually actually run the inverse matrix calculation.
## Args:
##   x - the input matrix to be cached.
## Returns:
##   The object with all four functions (setters and getters)  and data to be cached (variable 'm'). It could be matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the cache for new objects.
    m <- NULL
    set <- function(y) {
        ## every time we change the x value, the cache needs to be initialized.
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This method gets the inverse of matrix either getting it from cache or if it is not available it
## will calculate it (using solve) 
## Args:
##   x - the object which holds the user matrix and potentially the inverse
## Returns:
##   The inverse matrix of data passed during makeCacheMatrix is returned here.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    ## Check in cache, if found return it.
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## If not found in cache get the raw matrix and calculate the inverse and set it in cache
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse    
}
