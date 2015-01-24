## Matrix inversion is often a costly computation and this functions will attempt
## to cache the inverse of a matrix rather than compute it repeatedly
 
## The makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.   set the value of the inverse matrix
## 4.      get the value of the inverse matrix
 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) {
    inv <<- inverse
  }
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}
 
 
## The cacheSolve function calculates the inverse of the special "matrix" created with makeCacheMatrix.
## It first checks if the inverse has already been calculated and if yes,
## gets it from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value in the cache.
 
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}
 

