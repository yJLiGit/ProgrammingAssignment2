## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ##To begin with, the inverse has not been cacluated
        inverse <- NULL
        ##if the matrix has been reset, inverse reset to null instead of using cache
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        ##retrieve the original matrix, if inverse == NULL, 
        ##the inverse of the matrix has not been calculated or the matrix has been reset
        ##else, use the cache
        get <- function() x
        ##calculate inverse of the matrix and save to cache 
        setinverse <- function(ins) inverse <<- ins
        ##get the inverse of the matrix from cache 
        getinverse <- function() inverse
                
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve :This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if inverse has been set to cache , return the result from cache , else calculate the inverse of the matrix
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
