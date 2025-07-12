## The two functions — makeCacheMatrix and cacheSolve — work together
## to efficiently compute and cache the inverse of a matrix, avoiding redundant
## calculations.

## This function builds a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversion <- NULL
        
        set <- function(y){
                x <<- y
                inversion <<- NULL
        }
        get <- function() x
        
        setInverse <- function(inverse) inversion <<- inverse
        getInverse <- function() inversion
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function works with the special object created by makeCacheMatrix,
## and either: returns the cached inverse (if already computed), or
## computes it, caches it, and returns the result.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        if(!is.null(inverse)){
                message("getting cached matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        
        inverse
}
