## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this is an "object" function that creates a matrix and
## calculates the inverse and implements all of the
## requisite getters and setters

makeCacheMatrix <- function(x = matrix()) {
    ## initialize i to NULL
    ## this is the variable that will store the inverse matrix
    i <- NULL
    
    ## object setter for the matrix x
    ## i must also be re-initialized
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## object getter for the matrix x
    get <- function() {
        x
    }
    
    ## object setter for the inverse of x
    setinverse <- function(inv) {
        i <<- inv
    }
    
    ## object getter for the inverse of x
    getinverse <- function() {
        i
    }
    
    ## expose the setter and getter functions as a list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## This function receives an invertible makeCacheMatrix
## object as an argument and then returns the inverse
## matrix.  If the inverse has already been calculated,
## the function returns the cached inverse.  Otherwise,
## the inverse is calculated and returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## first fetch the existing value of the inverse matrix
    inv <- x$getinverse()
    
    ## if the inverse is NOT NULL then return the cached inverse matrix
    if (!is.null(inv)) {
        ## notify user that inverse matrix fetched from cache
        message("getting cached inverse matrix")
        ## return the cached value
        return(inv)
    }
    
    ## otherwise calculate the inverse matrix
    inv <- solve(x$get())
    ## cache the inverse matrix
    x$setinverse(inv)
    ## return the inverse matrix
    inv
}
