## The following two functions calculate/help calculate the inverse
## of a matrix

## The following matrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set,get = get,setInverse = setInverse, getInverse = getInverse)
}


## This functions computes the inverse of a matrix. If the inverse has already
## been calculated it gets the value directly from the cache,thus saving the 
## computation time.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse
}
