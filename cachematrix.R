# Two step process. Step 1, use makeCacheMatrix to create an object
# which will consist of a vector of objects that will hold information
# related to the matrix inverse

# The function cacheSolve will check to see if it already has the matrix inverse
# If so, will returned with stored value. If not, will compute new inverse

# To test, run cacheSolve for a matirx, will get inverse as output
# do it a second time, will get a "Retrieving cached data..." message
# followed by the inverse. For kicks, multiply matrix and inverse to 
# see if the indentity matrix results

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
matrix_inverse <- NULL # set matrix inverse to NULL
# print(c("input matrix",x))
# print("set y to null, and why prints out as")

      #  matrix_inverse is the inverse of the input matrix ‘x’ 
      #  matrix_inverse is reset to NULL each time time makeCacheMatrix is called
  set <- function(y) {
        x <<- y 
        matrix_inverse <<- NULL
        # caches the inputted matrix so that cacheSolve can check whether it has changed
      }
# print("set and updated x is equal to")
# print(c("set", set,"x matrixt",x))

      get <- function() {x}

# print(c("get",get, "type of get",typeof(get)))
      setinverse <- function(solve) {matrix_inverse <<- solve}
      getinverse <- function() {matrix_inverse}
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  #  print(set,get,setinverse,getinverse)
}

# ----------------
cacheSolve <- function(x, ...) {
  # This function will either return the cached matrix inverse, or compute and return a new one
  matrix_inverse <- x$getinverse()
  # print(c(matrix_inverse, typeof(matrix_inverse))) # see if inverse is correct
  if(!is.null(matrix_inverse)) {
    message("Retreiving cached data...")
    return(matrix_inverse) # if the inverse is not NULL, use the cached inverse
    # If the result is cached, will simply return cached inverse
  }
  data <- x$get()
  # typeof(data)
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse # if inverse was not cached, will compute and return
}


    
