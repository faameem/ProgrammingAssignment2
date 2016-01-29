## The assignment is to write a pair of functions that cache the inverse of a matrix.
## 
## (1) makeCacheMatrix: This function creates a special "matrix" object that can 
##     cache its inverse.
## (2) cacheSolve: This function computes the inverse of the special "matrix" 
##     returned by makeCacheMatrix above. If the inverse has already been 
##     calculated (and the matrix has not changed), then the cachesolve should
##     retrieve the inverse from the cache.


## This function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse of the matrix
## (4) get the value of the inverse of the matrix
## 
## Example:
## > m1<-matrix(data=c(1,2,3,4),2,2)
## > mcm<-makeCacheMatrix(m1)
makeCacheMatrix <- function(m = matrix()) {
    s <- NULL
    set <- function(y) {
        m <<- y
        s <<- NULL
    }
    get <- function() m
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
## The following function calculates the inverse (using Solve()) of the special "matrix" created with the
## [makeCacheMatirx()] function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the setsolve function.
##
## Example:
## cacheSolve(mcm)
cacheSolve <- function(m, ...) {
    s <- m$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- m$get()
    s <- solve(data, ...)
    m$setsolve(s)
    s
}
