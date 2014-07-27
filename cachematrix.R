## Both function below are to improve the performance at inverting a matrix in R. 
## Since it take time to calculate the inverted matrix, 
## it's wise to save the result of the inverted matrix in the cache in oder to 
## retrieved it quickly when is needed again and to avoid recalculation.

## MakeCacheMatrix function will store both the matrix and inverted matrix
## in the variable x$get and x$getinv respectably. In this way, 
## the retrieve of the inverted matrix is direct and there will be no need to do recalculation.


makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          
          set <- function(y) {
            x <<- y
            inv <<- NULL
          }
          get <- function() x
          
          setinv <- function(invMatrix) inv <<- invMatrix
          getinv <- function() inv
          list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve function will calculate the inverse matrix if it hasn't been calculdated before
## and store it in x$getinv using the makeCacheMatrix, otherwise it will retrieve the inverted matrix
## stored in the x$getinv. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Getting data from Cache")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
}
