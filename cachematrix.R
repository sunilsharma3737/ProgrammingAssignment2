## The functions defined below, i.e.makeCacheMatrix function creates a matrix that will be able to cache its inverse.

makeCacheMatrix <- function(a = matrix()) {
inv <- NULL
  setting <- function(b) {
    a <<- b
    inv <<- NULL
  }
  getting <- function() a
  settingInverse <- function(inverse) inv <<- inverse
  gettingInverse <- function() inv
  list(setting = setting,
       getting = getting,
       settingInverse = settingInverse,
       gettingInverse = gettingInverse)

}


## This function can compute the inverse of the matrix returned by makeCacheMatrix above. 
##If the inverse has already been calculated, then the cacheSolve will be able to remember and recover the inverse from the cache.

cacheSolve <- function(a, ...) {

  ## This function will return a matrix that will be the inverse of 'a'
  
  inv <- a$gettingInverse()
  if (!is.null(inv)) {
    message("cached data here")
    return(inv)
  }
  mat <- a$getting()
  inv <- solve(mat, ...)
  a$settingInverse(inv)
  inv
  
}
