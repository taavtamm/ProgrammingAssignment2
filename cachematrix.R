# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the inverse value of the matrix
# get the inverse value of the matrix

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(n) {
        m <<- n
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## Write a short comment describing this function

# This function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If it has then it gets the result and skips the
# computation. If not, it computes the inverse and sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

