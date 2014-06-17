## makeCacheMatrix and cacheSolve works together to allow you to cache the result of
## a potentially time consuming operation (matrix inversion)

##  makeCacheMatrix <- function(x = matrix())
## This function creates a special "matrix" object that can cache its inverse.
## Input:   x -> matrix to be cached
## Returns: a list with functions for getting and setting the values of the 
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # sets the value of the matrix and initializes its inverse
  Set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # gets the value of the matrix
  Get <- function() x
  
  # sets the value of the inverse matrix
  Setinverse <- function(inverse) m <<- inverse
  
  # gets the value of the inverse matrix
  Getinverse <- function() m
  
  list(set = Set, get = Get,
       setinverse = Setinverse,
       getinverse = Getinverse)
}


## cacheSolve <- function(x,...)
## It computes the inverse of a "matrix" 
## If the inverse has already been calculated (and the matrix has not changed), 
## cachesolve should retrieve the inverse from the cache.
## Input:   x -> matrix to be inversed
##          ... -> optional parameters to the solve function
## Returns: a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  # if inverse has been previously calculated return cached inverse
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  # else load the matrix and calculate its inverse
  data <- x$get()
  m <- solve(data, ...)
  
  # store the inverse calculation on cache
  x$setinverse(m)
  
  #return the inverse matrix
  m
}
