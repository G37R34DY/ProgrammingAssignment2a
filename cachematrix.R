## G37-R34DY - Programming Assignment
## Caching the inverse of a Matrix

## function makeCacheMatrix() creates a vector of functions

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## cacheSolve() calculates the inverse of a matrix being given the vector returned by makeCacheMatrix() and evaluates
## if the matrix has already been solved or not. If already solved, retrieves the value from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Retrieving from cache!")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

