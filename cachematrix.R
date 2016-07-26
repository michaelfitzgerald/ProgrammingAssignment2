## The functions will invert an (invertible) matrix. By caching the result the
## the cacheSolve function can retrive the result rather than recalculating. 

## This function uses the solve function to invert an invertible matrix and 
## make the result available in a parent environment.
makeCacheMatrix <- function(x=matrix()) {
    
    inverseMatrix <- NULL
    
    if(nrow(x) == ncol(x)) {
        set <- function(y) {
            x <<- y
            inverseMatrix <- NULL
        }
        
        get <- function() {x}
        setinverse <- function(solve) {inverseMatrix} <<- solve
        getinverse <- function() {inverseMatrix}
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    }
    
    else {
        print("Matrix is not invertible")
    }
    
}


## This function can compute the matrix inverse from the makeCacheMatrix() function.
cacheSolve <- function(x,...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("fetching cached result")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
