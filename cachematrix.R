## This function caches the inverse of a matrix
## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getMatrix <- function() {x}
  setInverse <- function(inverse) {i <<- inverse}
  getInverse <- function() {i}
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix created in 
## makeCacheMatrix or retrieves the inverse from the cache 
## if it has already been calculated

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$getMatrix()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}
