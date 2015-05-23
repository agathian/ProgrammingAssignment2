## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv  <-  NULL
  
  # set x to the input matrix, rest inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get returns the current value of x
  get <- function() x
  
  #setinv stores/caches the supplied value into inv
  setinv <- function(newinv) inv <<- newinv
  
  #getinv retuns the current cached value of inv
  getinv <- function() inv
  
  ## store all the functions into a list and return it
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # let us find the current cached value
  inv <- x$getinv()
  
  
  # if it is not null, i.e., has something - then return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## ok, if we are here, then no inv is cached, let us compute
  
  data <- x$get()
  inv <- ginv(data, ...)
  x$setinv(inv)
  inv
}
