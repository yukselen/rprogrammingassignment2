## This file includes a couple functions to create inverse of a matrix


## First function is to prepare the arguments for the second, i.e getters and setters: get, set, getinv and setinv 

makeCacheMatrix <- function(x = matrix()) {
        
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinv <- function(inv) m <<- inv
                getinv <- function() m
                list(set = set, get = get, setinv = setinv, getinv = getinv)
        }



## Second function calculates the inverse of x matrix, using arguments created in makeCacheMAtrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## solve function calculates inverse of a matrix
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
        
