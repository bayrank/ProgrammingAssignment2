
## Matrix inversion is usually a costly computation, therefore we create a pair of functions to 
## compute and cache the inverse of the matrix such that inverse will be calculated only once 

## This function creates wrapper object around matrix, which can cache inverse of the matrix
## Formal argument 'x' and local variables 'inv' are referenced by inner functions 'get', 'set', 'getinv' and 'setinv'
## (by lexical scope)
## Therefore object returned from the function ( a list) will still have access to the matrix and its cached inverse
## 
makeCacheMatrix <- function(x = matrix()) {
  
  # 'inv' will be matrix inverse and it is reset to NULL every 
  # time makeCacheMatrix is called or when the 'set' function is called send a new matrix
  inv <- NULL
  
  # sets a new matrix for which to cache to calculate inverse 
  # reinitializing 'inv' here will force reevaluation of the matrix inverse 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # this function returns the value of the original matrix. 'x' is the formal parameter accessible via
  # local environment inside makeCacheMatrix funcion
  get <- function() x
  
  # helper functions used to get and set inverse matrix
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  
  # a wrapper object containing above methods
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes wrapper object created by makeCacheMatrix and returns the cached inverse matrix
## if the cached value is not present, inverse will be calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the cached matrix inverse
  inv <- x$getinv()
  
  #check if inverse was already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #inverse was not cached, therefore calculate, cache and return the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
  
}
