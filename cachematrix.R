## functions to cache the inverse of a matrix as computing the inverse 
## of a matix can be computationally expensive. 

## function to create a special vector of get and set functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set x and inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #return x
  get <- function() x
  #set inv
  setinv <- function(inverse) inv <<- inverse
  #return inv
  getinv <- function() inv
  #special vector of get and set functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## function checks if the inverse has been computed, 
## if yes - the cached inverse is returned
## if not - computes the inverse
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
