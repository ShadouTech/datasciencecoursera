## Put comments here that give an overall description of what your
## functions do

## This function will take a matrix and calculate the inverse of it and cache that value away.

makeCacheMatrix <- function(z = matrix()) {
        m <- NULL 
        set <- function(y) { 
                x <<- y 
                m <<- NULL 
        } 
        ####set values for m and x outside in the global environment, outside the function.
        get <- function() z
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
#will define the function for calculating the inverse and allowing the second function to retrieve the cached value.

## This function will solve the inverse of the matrix only if the inverse had not already been previously cached. 
##The function first inspects for a cached value set by makeCacheMatrix and if that is found, returns that value instead of re-calculating the inverse.

cacheSolve <- function(z, ...) {
        m <- z$getinverse() 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } 
        data <- z$get()
        m <- solve(data, ...)
        z$setinverse(m)
        m
}
