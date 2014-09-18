## Based on the example functions; the first function defines the same four functions as the example:
## set puts the value of m as null and the value of x as the matrix of interest
## get calls x, the matrix
## setInverse sets the value of m as the solution found in the cacheSolve function
## getInverse calls m

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Again based on the example function, this defines m as the inverse if it has been previously
## derived. If x$getInverse is not defined from the previous function m = null, and the function
## will solve for the inverse and assign it to m. If m is defined from a previously run cacheSolve,
## the function will show the message and the previously assigned inverse value.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
