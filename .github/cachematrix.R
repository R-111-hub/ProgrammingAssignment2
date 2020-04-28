makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}




cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if (!is.null(xi)) {
    message("getting cached inverse")
    return(inverse)
  }
  xm <- x$get()
  inverse <- solve(xm, ...)
  x$setinv(inverse)
  return(inverse)
}
