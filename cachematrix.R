## This pair of functions create and cache the inverse of a matrix,
## as well as retrieve it from the cache.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inversed_x <<- solve(x)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  retrieved_x <- inversed_x
  if (!is.null(retrieved_x)){
    if (identical(retrieved_x, solve(x))){
      return (retrieved_x)
    }
  }
  makeCacheMatrix(x)
    
}
