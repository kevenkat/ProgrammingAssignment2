##There are two functions below. These functions return the inverse of a given matrix.
## It is assumed that we will be using inversible matrix as arguments for these functions.
## These functions below stores the inverse of a matrix in cache , and if the matrix is unchanged
## returns the inverse from cache.


## 1. makeCacheMatrix : This is a function to create a matrix object that 
##can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## 2.  cacheSolve: This function computes the inverse of the special
##"matrix" oject returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve`  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
