## This program will take in a matrix and return its inverse.

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


## cacheSolve takes in a list from makeCacheMatrix and Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        # from the matrix, trigger getInverse function to get the value of inverse
        inverse <- x$getInverse()
        
        # if the value is not null, means it has been called before
        # hence, getting from the cache and return the value
        if(!is.null(inverse)) {
                message("getting cached data.")
                return(inverse)
        }
        
        # else get the matrix using get function
        mat <- x$get()
        
        # using solve() function to generate the inverse matrix
        inverse <- solve(mat)
        
        # then set the inverse
        x$setInverse(inverse)
        
        # return the inverse value
        inverse
}
