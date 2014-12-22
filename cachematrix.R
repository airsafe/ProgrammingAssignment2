## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
matrix_inverse <- NULL  
      y <- NULL
      #  matrix_inverse is the inverse of the input matrix ‘x’ 
      #  matrix_inverse is reset to NULL each time time makeCacheMatrix is called
      set <- function(y) {
        x <<- y 
        matrix_inverse <<- NULL
        # caches the inputted matrix so that cacheSolve can check whether it has changed
      }
      get <- function() {x}
      setinverse <- function(solve) {matrix_inverse <<- solve}
      getinverse <- function() {matrix_inverse}
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    }




## Write a short comment describing this function

cacheSolve <- function () {
  cacheSolve <- function(x, ...) {
    # This function will either return the cached matrix inverse, or compute and return a new one
    matrix_inverse <- x$getinverse()
    if(!is.null(matrix_inverse)) {
      message("Retreiving cached data...")
              return(matrix_inverse)
              # If the result is cached, will simply return cached inverse
    }
              data <- x$get()
              matrix_inverse <- solve(data, ...)
              x$setinverse(matrix_inverse)
              matrix_inverse
              # If the result is not cached, will compute inverse of matrix and will return that new inverse
  }
}
            