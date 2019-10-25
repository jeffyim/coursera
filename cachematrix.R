
## Put comments here that give an overall description of what your
## functions do
## My function creates a cache for inverses of a matrix

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  makeCacheMatrix <- function(x = matrix()) { ## define the argument with x's default value being the matrix
    inv <- NULL                             ## inv will hold value of matrix inverse 
    set <- function(y) {                    ## set function will assign new y value of matrix in the parent environment
      x <<- y                             
      inv <<- NULL                        ## reset value of inv to NULL if new matrix
    }
    get <- function() x                     ## get function returns the value of the matrix
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## labelling for the functions to refer to
  }
  
  
  ## Write a short comment describing this function
  ## This function will find the inverse of a matrix from makeCacheMatrix function. If the inverse has been calculated before and the vectors are the same, cacheSolve will retrieve the value from the cache.
  
  cacheSolve <- function(x, ...) {        ## return an inverse of the matrix x
    
    inv <- x$getinverse()                 ## set the inv value to value of inverse
    if(!is.null(inv)) {                   ## if inv is NOT NULL, meaning previous value exists, show the message then retrieve the inv value.
      message("getting cached data")
      return(inv)
    }
    invdata <- x$get()                       ## if inv value is NULL, meaning no previous value, get the original matrix data 
    inv <- solve(invdata, ...)               ## use solve function to inverse the matrix
    x$setinverse(inv)
    inv   
    
  
  }
}
  
  
  
  