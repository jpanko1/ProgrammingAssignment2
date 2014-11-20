## This pair of functions can be used to calculate the inverse of a matrix
##  once, cache it, and then retrieve it when it's needed later.  Calculating
##  the inverse can be time consuming, so it's a good idea to calculate it
##  just once.

## Returns a matrix that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(anInverse) inverse <<- anInverse
  getinverse <- function() inverse

  m <- matrix( c( set, get, setinverse, getinverse ), nrow=2, ncol=2 )
  dimnames(m) <- list( c("set","get"), c("matrix","inverse") )
  m
}


## Returns inverse of the input matrix.  If the inverse was already calculated,
##  it returns the inverse from the cache; otherwise, it calculates 
##  the inverse, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x[["get","inverse"]]()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x[["get","matrix"]]()
  inv <- solve(data, ...)
  x[["set","inverse"]](inv)
  inv
}
