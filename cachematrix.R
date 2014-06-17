# makeCacheMatrix: return a list of functions to:
# 1) Set value of the matrix
# 2) Get value of the matrix
# 3) Set value of the inverse
# 4) Get value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # store cached inverse matrix
  inverse <- NULL
  
  # Set matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # Get matrix
  get <- function() x
  
  # Set inverse
  setinverse <- function(inverse1) inverse <<- inverse1
 
  # Get inverse
  getinverse <- function() inverse
  
  # Return the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  # return inverse if already calc'ed
  if (!is.null(inverse)) {
    message("cached data, get:")
    return(inverse)
  }
  
  # If not cached, calculate inverse
  data <- x$get()
  inverse <- solve(data, ...)
  
  # Cache inverse
  x$setinverse(inverse)
  
  # Return inverse
  inverse
}
test <- matrix(rnorm(9), nrow = 3, ncol = 3)
inversetest <- makeCacheMatrix(x)  
inversetest$get()
cacheSolve(inversetest)
cacheSolve(inversetest)

## resource used: R-R- github repo for guidance. This is my first time coding. Assignment mostly over my head without some written code to dissect and understand. Thanks!