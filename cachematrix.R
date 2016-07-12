## Below function help in caching matrix inverse of a matrix and speeding up the process of calculating inverse.

## This function constructs a matrix thet needs to be cached.

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(matInv) mInv <<- matInv
  getInverse <- function() mInv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function checks in cache whether inverse is already calculated otherwise calculate it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInv <- x$getInverse()
  if(!is.null(mInv)) {
    print("getting cached data")
    return(mInv)
  }
  
  m <- x$get()
  mInv <- solve(m, ...)
  x$setInverse(mInv)
  mInv
}
