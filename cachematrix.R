# cfvandev 22/06/2014
# Caches the inverse of a matrix so that if the matrix does not change
# the inverse can be looked up from the cache instead of re-calculated



makeCacheMatrix <- function(x = matrix()) {

# creates a matrix that is used to store the cached inverse     
# takes as argument the matrix to be inverted and returns the object that 
# can cache the matrix     
        
        m <- NULL #default value
        set <- function(y) {
        #The "<<-" operator used to set variable that already exists 
        # in the parent environment.
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) m <<- solve
        
        getinverse <- function() m
        
        # create the list to access properties with $ sign. 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)       
}


cacheSolve <- function(x, ...) {
        # Calculates the inverse of the matrix, but first checks
        # to see if the inverse has already been calculated.
        # If so, it returns the cached value 
        
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