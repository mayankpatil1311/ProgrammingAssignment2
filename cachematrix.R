## Put comments here that give an overall description of what your
## functions do
## These functions written in partial fulfillment of Coursera Data Science: R Programming 
## Week 3 Assignment;

## Write a short comment describing this function
## library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
  ## This function creates a special "matrix" object that can cache its inverse
  inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {                    ## define the set function to assign new 
    x <<- y                             ## value of matrix in parent environment
    inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                    ## define the get fucntion - returns value of the matrix argument
  
   setinv <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinv <- function() ## gets the value of inv where called
  {
    inver<-ginv(x)
    inver%*%x
    }
  list(set = set, get = get, setinv = setinv, getinv = getinv)  ## you need this in order to refer 
  ## to the functions with the $ operator
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
