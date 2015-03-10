## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function return an object stores the inversion of matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
    ## inversion of 'x'
    inv_x <- NULL
    
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinv <- function(inv) {
        inv_x <<- inv
    }
    getinv <- function() inv_x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## if function makeCacheMatrix has the inversion, return it,
## otherwise, calculate the inversion, transfer it to 
## makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
