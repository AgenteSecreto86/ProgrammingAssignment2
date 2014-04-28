## The two functions in this file work togheter. The idea is to define a new 
## matrix object that caches its own inverse if it was calculated previously.
## Matrix inversion is usually a costly computation and there may be some 
## benefit of caching the inverse in in some situations, in particularly inside a loop.

## makeCacheMatrix(): This function creates a special "matrix" object that can
## cache its inverse. It is assumed that the supplied matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
        
        # define an a variable (initially NA) to hold the inverse of matrix x.
        inv <- NULL
        
        # functions for setting and getting the internal matrix object x.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        # functions for seting and getting the value of the inverse matrix.
        setInv <- function(newInv) inv <<- newInv        
        getInv <- function() inv
        
        # return the special "matrix" object consisting of list of functions.
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
}

## cacheSolve(): This function computes the inverse of the special "matrix" 
## return by makeCacheMatrix() above. If the inverse has already been computed  
## (and the matrix has not changed), then it returns the cached value.

cacheSolve <- function(x, ...) {
        # returns a matrix that is the inverse of x. Uses the inverse matrix 
        # cached in x, if any.
        inv <- x$getInv()        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # here the inverse has not been calculated previously so 
        # we proceed to solve it from the matrix data stored in x.
        data <- x$get()
        inv <- solve(data, ...)
        
        # finally, we store the inv into the special "matrix" x and
        # return the calculated value
        x$setInv(inv)
        inv
}