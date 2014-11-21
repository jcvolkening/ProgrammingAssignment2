## To save computation time, these functions will 
## allow someone to cache the inverse of a matrix, 
## instead of computing it over and over again.

## This function allows you to set/get the value of
## the matrix, and set/get the inverse value of the
## matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function prints the inverse of the matrix. If 
## that has already been computed, it gets that result,
## prints it and does not compute the inverse again. If
## it has not already been computed, it will compute the 
## inverse of the matrix, set that in the cache, and print
## the result.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
