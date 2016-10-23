## The functions defined below, i.e.makeCacheMatrix function creates a matrix that will be able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  setting <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getting <- function() x
  settingInverse <- function(inverse) inv <<- inverse
  gettingInverse <- function() inv
  list(setting = setting,
       getting = getting,
       settingInverse = settingInverse,
       gettingInverse = gettingInverse)

}


## This function can compute the inverse of the matrix returned by makeCacheMatrix above. 
##If the inverse has already been calculated, then the cacheSolve will be able to remember and recover the inverse from the cache.

cacheSolve <- function(x, ...) {

  ## This function will return a matrix that will be the inverse of 'x'
  
  inv <- x$gettingInverse()
  if (!is.null(inv)) {
    message("cached data here")
    return(inv)
  }
  mat <- x$getting()
  inv <- solve(mat, ...)
  x$settingInverse(inv)
  inv
  
}
