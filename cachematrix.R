## The following two functions work together to create a square matrix 
## and make the inverse of the matrix available in the cache environment

## makeCacheMatrix creates a list containing a function to create the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_inverse <<- inverse
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the cacheSolve function returns the inverse of the matrix returned by the makeCacheMatrix function 
## from the cache previously calculated

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
