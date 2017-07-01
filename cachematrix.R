## Put comments here that give an overall description of what your
## functions do

## 'makeCacheMatrix' creates a matrix to be used later in  the 'cachsolve' function:
## it allows retrieving the inverse matrix without calculating it, if it has been 
## calculated already by 'cachsolve'

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL ##
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

## 'cachesolve' takes the "matrix" assigned by 
## makeCacheMatrix above and calculates its inverse using the solve() function.
## if a new matrix hasn't been assigned, then cachesolve retrieves the inverse 
## from the cache (from 'makeCacheMatrix') .

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


