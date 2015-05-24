## To cache inverse of a matrix


## This function creates a spectial vector with list of functions to cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv  <-  NULL
  
  # set x to the input matrix, reset inv
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


## Returns the current cached value or computes the inv

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
  inv <- solve(data)
  x$setinv(inv)
  inv
}
