## This program defines a way to make a matrix and compute its inverse that
## caches and reuses the solution whenever possible, reducing computation time


## a function to make a specialized "matrix" that can cache the value of its 
## inverse, reducing computation time
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        # set functionalities
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        setinv <- function(inv) inverse <<- inv
        
        # get functionalities
        get <- function() x
        getinv <- function() inverse
        
        # return statement including all of these data
        list(set = set, setinv = setinv, get = get, getinv = getinv)
}


## a function to calculate the inverse of a "matrix" (from the above function)
## tries to find the cached solution, otherwise does the computation
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        # attempt to find a cached value
        if (!is.null(inv)) {
                print("getcached")
                return(inv)
        }
        # no cache value exists, so we must perform the computation
        rawmat <- x$get()
        inv <- solve(rawmat, ...) # assume the matrix is invertible
        x$setinv(inv) # cache that value for next time
        return(inv)
}
