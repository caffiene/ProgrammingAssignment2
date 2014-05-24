## The functions makeCacheMatrix and cacheSolve collectively
## create a matrix object capable of computing and caching 
## its inverse.

## makeCacheMatrix
## Purpose: This function creates a special "matrix" object 
##    that can cache its inverse.
## Inputs:
##    x - Matrix assumed to be invertible.
## Return: List containing functions to set the matrix, get the
##    matrix, set the inverse of the matrix, and get the inverse
##    of the matrix.  When setting the matrix, the inverse is set
##    to NULL.


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(z) inverse <<- z
    getinverse <- function() inverse
    
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
## Purpose: This function computes the inverse of the special 
##   "matrix" returned by makeCacheMatrix above. If the inverse 
##   has already been calculated (and the matrix has not changed), 
##   then the cachesolve should retrieve the inverse from the 
##   cache.
## Inputs:
##    x - Special 'matrix' returned by makeCacheMatrix
## Return: Inverse of the matrix stored in x.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        
        if (!is.null(inverse)){
           message("getting cached inverse")
           return(inverse)
        }
        message("calculating inverse")
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        return(inverse)
}