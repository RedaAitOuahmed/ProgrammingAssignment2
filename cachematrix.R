## makeCacheMatrix creates an object that allows to cache data about the matrix
## cacheSolve solve the inverse of a makeCacheMatrix object
## it tries to find a cached inverse, if it can't it solves the inverse and cache it

## returns a list of functions to get or set the matrix
## and to get or set the Inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve returns the argument matrix's inverse
## it checks first if it was already calculated and in this case 
##        returns the cached inverse matrix
## else it calls solve to get the matrix inverse and then cache the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inversed matrix")
    return(inverse)
  }
  message("no cached inversed matrix")
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}
