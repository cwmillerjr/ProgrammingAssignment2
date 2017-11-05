## These functions utilize a special caching object wrapping 
## the inversion of matrices

#' @description The function makeCacheMatrix creates
#'  a cache object for a matrix
#' @param x matrix matrix
#' @return list containing four functions, get, set, getinverse, and setinverse
#' @examples 
#' mtrx <- matrix(c(2,4,0,6,8,3,1,5,7), nrow=3, ncol=3)
#' example <- makeCacheMatrix(mtrx);
#' i <- example$getinverse()
#' if (is.null(i)) {
#'   example$setinverse(solve(example$get()))
#'   i <- example$getinverse()
#' }

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' @description The function cacheSolve takes a matrix cache object 
#' and returns the inverse of the matrix in the cache object, 
#' computing it only if it is not already available.
#' @param x a matrix cache object created by \code{link\{makeCacheMatrix}}
#' @return inverse of matrix in supplied cache object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
