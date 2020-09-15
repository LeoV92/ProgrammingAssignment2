## Programming Assignment 2: Lexical Scoping
# For this assignment I am going to write a couple of functions that cache 
# the inverse of a  special matrix.

## The makeCacheMatrix will turn a given matrix into an special matrix object
## and cache it with its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Matrix_Inv <- NULL
  set <- function(y) {
    x <<- y
    Matrix_Inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) Matrix_Inv <<- inv
  getinv <- function() Matrix_Inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function computes the inverse of the matrix created 
## by makeCachematrix and if the inverse has already been computed 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  Matrix_Inv <- x$getinv()
  if(!is.null(Matrix_Inv)) {
    message("getting cached data")
    return(Matrix_Inv)
  }
  data <- x$get()
  Matrix_Inv <- inv(data, ...)
  x$setinv(Matrix_Inv)
  Matrix_Inv
}
