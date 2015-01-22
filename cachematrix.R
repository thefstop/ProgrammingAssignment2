## Program: cachematrix.R
## Author: thefstopshere
## Programing assignment 2 for the Coursera course R Programming
## 
##
## "If you can't fly then run, if you can't run then walk, 
## if you can't walk then crawl, but whatever you do you have
## to keep moving forward."  - MLK
##
## This program calcualtes the inverse of a special 'matrix' created with
## the makeCacheMatrix function.  The inverse of the matrix is cached, allowing
## the inverted matrix to be accessed at any time without having to recalculate.
##
##


## Function: makeCacheMatrix
##
## This function is used to create the 'matrix' type object that will store
## the inverse of a matrix.  After creating calling this function on a matrix,
## the cacheSolve function can be used to obtain the inverse of the matrix.
##
makeCacheMatrix <- function(x = matrix()) {
  
  invert <- NULL

  set <- function(z) {
    x <<- z
    invert <<- NULL
  }
  get <- function() x
  setInvert <- function(cacheSolve) invert <<- cacheSolve
  getInvert <- function() invert
  getevn<- function() environment()
  list(set = set, get = get,
       setInvert = setInvert,
       getInvert = getInvert)

}

## Function: cacheSolve
##
## This function is used to obtain the inverse of a matrix object created with the
## makeCacheMatrix function.  Ifthe inverse of the matrix has already been calcualted
## previously, the function will return the cached value along with a descriptive and 
## motivating message.

cacheSolve <- function(x, ...) {
 
  invert <- x$getInvert()
  if(!is.null(invert)) {
    message("Getting cached data... way to go! You're saving time!")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setInvert(invert)
  invert
        
}
