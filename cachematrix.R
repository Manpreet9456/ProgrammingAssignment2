makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This function computes the inverse of the "matrix" makeCacheMatrix. 
##If the inverse has been previously calculated and the matrix has not been changed, the inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
## First we can create a "matrix" with the numbers from  and create cache and retrieve the inverse

mymatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
mymatrix$get()
cacheSolve(mymatrix)
cacheSolve(mymatrix)
mymatrix$getInverse()
mymatrix$getInverse()
