## Put comments here that give an overall description of what your
## functions do

##==============================================================================
## The purpose of this script is to calculate the inverse of a  given matrix
## utilizing R's powerful concept of storing an object in a different environment
##==============================================================================

## Write a short comment describing this function
##==================================================================================
## The function makeCacheMatrix creates a matrix and stores it in cache. It contains
## four other functions: set, get, getinverse, and setinverse. Description of each
## one can be found below.
##==================================================================================

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {   ## changes the matrix stored in the main function
    x <<- y
    i <<- NULL
  }
  get <- function() x  # returns the matrix stored in the main function
  setinverse <- function(inverse) i <<- inverse ## stores the value of the input in a variable "i"
  getinverse <- function() i
  list(setinverse = setinverse, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##==========================================================================================
## The function cacheSolve calculates the inverse of the matrix created with makeCacheMatrix
##==========================================================================================
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
