source(C:\Users\nishan.maharaj\Desktop\ProgrammingAssignment2-master)
## Caching the Inverse of a Matrix:
## Below are a pair of functions that are used to create a "special object" that 
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment.e
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

## This function (cacheSolve) computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## otherwise, calculates the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
x = makeCacheMatrix(matrix(1:4, 2, 2))
x$get()
x$getInverse()
cacheSolve(x)
x$getInverse()
