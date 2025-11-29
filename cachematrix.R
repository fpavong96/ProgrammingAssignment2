## Asuming matrix is always invertible
makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  # set a new matrix
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
    # get the matrix
  get <- function() x
    # set the inverse
  setInverse <- function(inverse) matinv <<- inverse
    # get the inverse
  getInverse <- function() matinv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Cache Solve the invex matrix

cacheSolve <- function(x, ...) {
  matinv <- x$getInverse()
  
  mat <- x$get()
  matinv <- solve(mat, ...)
  
  # Cache the inverse
  x$setInverse(matinv)
  
  matinv
}
