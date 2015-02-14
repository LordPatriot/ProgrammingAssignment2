## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    x_inverse = NULL
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    
    get <- function() {
        return(x)
    }
    
    setinverse <- function(inverse) {
        x_inverse <<- inverse
    }
    
    getinverse <- function() {
        return(x_inverse)
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(x$get())
    x$setinverse(inverse)
    inverse
}
