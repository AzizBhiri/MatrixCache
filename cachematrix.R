## This codeis used to cache the inverse of a matrix

## This function will create a matrix object that will cache itself

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL

    set <- function( matrix ) {
            x <<- matrix
            inv <<- NULL
    }

    get <- function() {
    	x
    }

    setInverse <- function(inverse) {
        inv <<- inverse
    }

    getInverse <- function() {
        inv
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#Calculate the inverse of the special matrix produced by "makeCacheMatrix" in the previous section.
#If the inverse has previously been calculated, the "cachesolve" function should get it from the cache.

cacheSolve <- function(x, ...) {
    mx <- x$getInverse()

    if( !is.null(mx) ) {
            return(mx)
    }

    data <- x$get()
    mx <- solve(data, ...)
    x$setInverse(mx)
    mx
}
