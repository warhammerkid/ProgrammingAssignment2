## The following functions cache matrix inversions so that they only need to be computed
## once for any given matrix.

## This creates a matrix wrapper that can store a cached inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL # Reset cached inverse, as matrix has changed
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Given an object from makeCacheMatrix, returns the inverse of the matrix.
## This only performs the matrix inversion if it isn't cached already.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
