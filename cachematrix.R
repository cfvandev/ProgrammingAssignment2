## Caches the inverse of a matrix so that if the matrix does not change
## the inverse can be looked up from the cache instead of re-calculated



makeCacheMatrix <- function(x = matrix()) {
## creates a matrix that is used to store the cached inverse     
       
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


cacheSolve <- function(x, ...) {
        ## Calculates the inverse of the matrix, but first checks
        ## to see if the inverse has already been calculated.
        ## If so, it returns the cached value 
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}