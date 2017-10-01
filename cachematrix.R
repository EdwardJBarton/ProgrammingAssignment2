## These functions work together to save having to needlessly 
## reapeat the computation of matrix inversions, e.g. in a loop,
## storing the computated inverse (if it doesn't already exist)
## in the cache which can be re-called later

## This function creates special "matrix" object that can cache
## its inverse. It returns a list of functions which:
## > sets the value of the matrix, if it doesn't already exist
## > gets the value of the matrix, if it already exists
## > sets the value of the matrix inverse, if it doesn't already exist
## > gets the value of the matrix inverse, if it already exists
## depending on which argument is included

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseM) i <<- inverseM
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function works with the output of makeCacheMatrix.
## It produces the inverse of the matrix, or if the inverse
## already has been calculated it gets it from the cache and
## skips the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
