## R Programming Assignment 2. Cache the inverse of a matrix.

## Create a matrix object

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
      x <<- y
      inverseMatrix <<- NULL
  }
  get <- function() x
  
  setInverse <- function(i) inverseMatrix <<- i
  getInverse <- function() inverseMatrix

  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  
  # Compute the inverse from the original matrix
  theMatrix <- x$get()
  theInverse <- solve(theMatrix, ...)
  x$setInverse(theInverse)
  
  theInverse
}
