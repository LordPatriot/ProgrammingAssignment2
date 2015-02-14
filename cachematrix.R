## This function creates an "enriched" matrix, which is a list containing 4 functions
makeCacheMatrix <- function(x = matrix()) {
    # initially the inverse matrix is set to NULL
    x_inverse = NULL
    
    # This function (re)sets the matrix
    set <- function(y) {
        # update the value of the matrix
        x <<- y
        
        # set the inverse to NULL (because it is not computed yet)
        x_inverse <<- NULL
    }
    
    # This functions returns the current value of the matrix
    get <- function() {
        return(x)
    }
    
    # This function (re)sets the inverse matrix
    setinverse <- function(inverse) {
        x_inverse <<- inverse
    }
    
    # This functions returns the current value of the inverse matrix
    getinverse <- function() {
        return(x_inverse)
    }
    
    # return a list with 4 functions in it
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function calculates the inverse matrix of a matrix
## [The assumption is that matrix is always invertible]
cacheSolve <- function(x, ...) {
    
    # get the current value of the inverse matrix
    inverse <- x$getinverse()
    
    # if it has copmputed - return the value instead of computing it again
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # if it hasn't been computed: get the matrix, compute the inverse and set the inverse matrix 
    matrix <- x$get()
    inverse <- solve(x$get(), ...)
    x$setinverse(inverse)
    
    # return the inverse matrix
    inverse
}
