## These functions take advantage of lexical scoping to cache the inversion of a 
## matrix to save run time if the same inversion is later needed.

## makeCacheMatrix creates a set of functions: set, get, setinversion, getinversion 
## in a list to allow for cacheing of a matrix inversion

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y){
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinversion <- function(z) mi <<- z
        getinversion <- function() mi
        list(set = set, get = get,
             setinversion = setinversion, 
             getinversion = getinversion)
}


## cacheSolve returns a matrix that is the inverse of x.  Pulls from the cached
## inversion if it has already been calculated.

cacheSolve <- function(x, ...) {
        mi <- x$getinversion()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        dat <- x$get()
        mi <- solve(dat, ...)
        x$setinversion(mi)
        return(mi)
}
