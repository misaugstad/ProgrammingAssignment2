## This file contains a pair of R functions that allow one to solve for the
## inverse of a matrix while caching it so it need only be computed once

## Wraps a matrix in a list containing functions
## to get and set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  # return set of four functions as wrapper for matrix x
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of a matrix wrapped by the makeCacheMatrix function.  If
## the inverse is not cached, it is solved for and cached. If the inverse was
## already cached, just return the cached version.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  ## null value indicates inverse not cached, so we compute and cache it
  if(is.null(inv)){
    m <- x$get()
    inv <- solve(m) ## solve computes inverse
    x$setInverse(inv) ## setInverse caches the inverse
    return(inv)
  }
  ## if index was cached, we just return it
  message("getting cached inverse")
  return(inv)
}
