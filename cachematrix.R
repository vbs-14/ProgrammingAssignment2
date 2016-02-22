## These two functions create a special object that stores a matrix and cache's
## its inverse.

## Example:
##
## >mymatrix<-matrix(rnorm(9),3,3)
## >espmatrix<-makeCacheMatrix(mymatrix)
## >invmatrix<-cacheSolve(espmatrix)
##


## This function creates a special "matrix" object containing a list of 4 
## to 1) set the value of the matrix.  2) get the value of the matrix. 
## 3) set the value of the inverse and 4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix stored in the 
## special "matrix" object created with makeCacheMatrix. 
## It checks whether the inverse has already been computed, and if so, 
## returns the stored value. If not, computes the inverse and returns the result.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
