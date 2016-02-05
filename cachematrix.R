## These functions create an object to store information about a matrix 
## and the capacity to find the matrix inverse within the object or to 
## independently determine the inverse.

## makeCacheMatrix creates an list of functions to get and set the values
## of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invMat <<- inverse
    getinv <- function() invMat
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the cached inverse matrix if it exists, or computes the
## inverse of a matrix otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMat <- x$getinv()
    if(all(is.na(x))) {
        message("getting cached data")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(x)
    x$setinv(invMat)
    invMat
}
