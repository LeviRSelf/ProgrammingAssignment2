## makeCacheMatrix creates a special "matrix" object which can be used
## to cache to cache its inverse
## cacheSolve computes the inverse of the special "matrix",
## retrieving the inverse from cache if possible

## This function creates a special "matrix" object with getters
## and setters for the underlying matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function retrieves the inverse from cache
## or calculates it from scratch if not in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
