

## This function creates a special "matrix" object that can cache its inverse.

## It has 4 functions to set, get the value of the matrix

## and set get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
   
  ## initializing Null
  
  m <- NULL
  
  ## set the inverse
  
  setmatrix <- function(matrix) m <<- matrix
  
  ## get the inverse
  
  getmatrix <- function() m
  
  ## get the value of the matrix
  
  get <- function() x
  
  ## set the value of the matrix
  
  set <- function(y) {
    
    x <<- y
    
    m <<- NULL
    
  }
  
  ## return a list with functions
  
  
  list(setmatrix = setmatrix,
       
       getmatrix = getmatrix,
       
       get = get, set = set)                           
  
}

## This function calculates the inverse of the

## "special matrix". It first checks if the inverse

## had already been calculated. If so it gehts the

## inverse from the cache an skips the calculation.

## Otherwise the inverse is calculated and set into

## the cache.


cachesolve <- function(x, ...) {
   
  m <- x$getmatrix()
  
  
  ## checks if already calculated
  
  if(!is.null(m)) {
    
    message("getting cached data")
    
    return(m)
    
  }
  
  
  ## if not get the matrix and calculate the inverse
  
  data <- x$get()
  
  m <- solve(data)
  
  ## and set the outcome into the cache               
  
  x$setmatrix(m)
  
  m
  
}