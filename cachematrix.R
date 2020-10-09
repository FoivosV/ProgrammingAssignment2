## A function that takes a matrix and caches its inverse
## and a function that returns the calculated inverse from the first function

## Take a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse= getInverse)
}


## get the cahced or calculate the inverse of a square invertible matrix

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
          message("getting cached inverse matrix")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
