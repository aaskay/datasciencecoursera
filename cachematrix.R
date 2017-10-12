# To use these functions, first make a call to makeCachedMatrix like the following:
# A <- matrix(c(1,3,2,4),nrow = 2,ncol = 2)
# myMatrix <- makeCachedMatrix(A)
#
# Then use the cachedSolve function as:
# cachedSolve(myMatrix)
#
# The first call actually calculates the inverse of the matrix A
# and subsequent calls gets the cached value


## Caches the inverse of a given (invertable) matrix x

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function will return the cached inverse of matrix x if it exists else it will compute the inverse.
## Note that the argument to this function is a list returned by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
