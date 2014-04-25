## Some of the operations such as matrix inversion
## in R may be time consuming. In such instances, we
## can make use of the lexical scoping in R, which
## allows for resolving the free variable bindings
## by first looking in the enviornmnet in which the
## fuction was first created.Therefore, caching the
## inverse of a matrix is faster than computing it
## repeatedly, for example, using forloops.
## For this Assignment2, I have created two
## functions, namely,makeCacheMatrix and cacheSolve.
 

## A short comment on `makeCacheMatrix`:
## This function creates a special
## "matrix" object that can cache its inverse. The 
## function takes matrix of nrow X ncol as an argument,
## and the values returned are a list of functions.

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) a <<- solve
    getInverse <- function() a
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## A short comment on 'cacheSolve':
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. As
## directed in the assignment, function solve() is used
## for computing the inverse of the squared matrix.

cacheSolve <- function(x, ...) {
    a <- x$getInverse()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    Invmatrix <- x$get()
    a <- solve(Invmatrix, ...)
    x$setInverse(a)
    a
}
