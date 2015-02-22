## These functions compute the inverse of a given matrix.
## The function cacheSolve will first check for a cache
## of the inverse matrix before computing it.


## This function creates a matrix object that can cache its inverse:
## it is a list containing functions that sets then gets the value of 
## the matrix and then does the same for the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)
}


## This function computes the inverse of the matrix but first
## checks for a cache of the solution

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
