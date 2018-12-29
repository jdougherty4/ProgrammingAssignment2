## makeCacheMatrix and cacheSolve create a chache of inverted matrices. 
## Since looping through Matrices and inverting them can be time consuming,
## It makes sense to create a cache which stores each inverted Matrix for future use.

## makeCacheMatrix creates an object which can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  #  browser()
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve checks if the inverse is cached, returns inverted matrix.

cacheSolve <- function(x, ...) {
  #  browser()
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
