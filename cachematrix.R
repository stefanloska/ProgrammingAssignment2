# This is a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    # Creates a special "matrix" object that can cache its inverse.
    # 
    # Arguments
    # x - the matrix whose inverse is to be cached (class "matrix)
    # 
    # Value
    # A list of functions:
    # set - set the value of the matrix
    # get - get the value of the matrix
    # setinverse - set the value of the inverse
    # getinverse - get the value of the inverse
    
    # inverse initialised as NULL
    i <- NULL
    
    # set function
    set <- function(y) {  # take the matrix
        x <<- y           # and store it 
        i <<- NULL        # reset the inverse to NULL (i) (parent environment)
    }
    
    #get function
    get <- function() x  # return the matrix
    
    # setinverse function
    setinverse <- function(inverse) i <<- inverse  # take the inverse and store it
    # (parent environment)
    
    # getinverse function
    getinverse <- function() i  # return the inverse
    
    # return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # Computes the inverse of the special "matrix" returned by makeCacheMatrix.
    # Uses the solve function
    # If the inverse has already been calculated (and the matrix has not changed),
    # then the cachesolve retrieves the inverse from the cache.
    #
    # Arguments
    # x - the the special "matrix" returned by makeCacheMatrix
    # ... - other arguments to be passed to the solve() function
    # 
    # Value
    # An object of class "matrix" being and iverse of the given matrix.  
    
    i <- x$getinverse()     # get the inverse already stored in x
    if(!is.null(i)) {       # if the inverse is not NULL 
        message("getting cached data")
        return(i)           # return it and exit
    }
    data <- x$get()         # othervise get the value of the matrix
    i <- solve (data, ...)  # and solve it (create the inverse)
    x$setinverse(i)         # set the value of the inverse in the x object (cache)
    i                       # return the inverse
}
