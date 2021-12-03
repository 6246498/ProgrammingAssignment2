## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  # A function to
  # set the vector, x, to a new vector, y, and
  # resets the mean, m, to NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  # Gets the vector, x
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  # Create a new object by returning a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  # retrieve a inverse from the object passed in as the argument.
  inv <- x$getInverse()
  # If the value here is not equal to NULL,
  # it has valid inverse and can return it to the parent environment
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # If the result of !is.null(inv) is FALSE
  # gets the vector from the input object, calculates inverse
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
