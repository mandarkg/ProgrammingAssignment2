## This program is written to calculate the inverse of a matrix. 
## There are two functions in this program

## The function makeCacheMatrix will be used to store the 
## matrix passed in function argument into the system cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve will be used to calculate inverse of matrix
## During function execution, it first checks the existence of inverse of matrix
## in the system cache. If the value exists in cache, it reurns that value and 
## computation of inverse is skipped

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
