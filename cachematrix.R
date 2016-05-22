## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # matrix in x, function copies matrix to x
  my_inv <- NULL
 
  set <- function(y) {
  x <<- y
  my_inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) my_inv <<- inverse
  
  getinverse <- function() my_inv
  
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  my_inv <- x$getinverse()
  if(!is.null(my_inv)) {
    message("getting cached data")
    return(my_inv)
  }
  
  m.data = x$get()
  my_inv = solve(m.data, ...)
  
  x$setinverse(my_inv)
  
  return(my_inv)
}
