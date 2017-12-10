## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is a function that takes in a matrix as an object
## and chaches its invers for the input - which is an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<-y
        inv <<- NULL
    }
    get <-function () x
    setinv <- function (inverse) inv <<- inverse
    getinv <- function () inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## The function below computes the inverse of a special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Getting the results...")
        return(inv)
    }
    a <- x$get()
    inv <- solve(a,...)
    x$setinv(inv)
    inv
}
