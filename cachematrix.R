## Put comments here that give an overall description of what your
## functions do

## "makeCacheMatrix creates a special "matrix" object that can cache its inverse"

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##################################

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cachesolve <- function(mat, ...) {
  inv <- mat$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setsolve(inv)
  inv
}


