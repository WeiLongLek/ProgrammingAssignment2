## Put comments here that give an overall description of what your
## functions do

## makecacheMatrix will take in an exisiting matrix and output the list functions for the matrix

makeCacheMatrix <- function(x = matrix()) {
        # initialize a variable to null first
        inverse <- NULL
        
        # this function will set the input y to the x and initialize inverse = null as well.
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # this function will return the value of x
        get <- function() x
        
        # this function will set the value of inverse to the inputInverse
        setInverse <- function(inputInverse) inverse <<- inputInverse
        
        # this function will return the value of inverse
        getInverse <- function() inverse
        
        # being the last value of the function, it will be returned. 
        # this returns a list of functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data.")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat)
        x$setInverse(inverse)
        inverse
}
