
## The code below contains pair of functions that 
## create a cache marix object that can be used to
## solve the inverse of the marix without 
## having to compute the inverse every single time


##This function creates a cacheMatrix object for an invertable matrix

makeCacheMatrix <- function(x = matrix()) {
  cacheInverse <- NULL
  set <- function(y) {
    x <<- y
    cacheInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cacheInverse <<- inverse
  getInverse <- function() cacheInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseFunction <- x$getInverse()
  if(!is.null(inverseFunction)) {
    message("getting cached data")
    return(inverseFunction)
  }
  data <- x$get()
  inverseFunction <- solve(data, ...)
  x$setInverse(inverseFunction)
  inverseFunction 
}
