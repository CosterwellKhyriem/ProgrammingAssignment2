## The following R code is to create two functions, which are:
## 1. makeCacheMatrix : This function takes a matrix as an input. It will cache the inverse of a matrix returned from the function below.
## 2. cacheSolve : This function will take the output from makeCacheMatrix. It shall first find if an inverse of the matrix exist else it shall compute the inverse using solve function inside makeCacheMatrix.

## This function takes a matrix as an input.
## It will return a special matrix where information on it's inversion has been calculated or not.
## It simply cache the information without performing any calculations
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    }
  get <- function() x
  setMatrixInversion <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setMatrixInversion = setMatrixInversion,
       getInverseMatrix = getInverseMatrix)
}

## This function will take the output from the function above.
##It will confirm if a cached inversed matrix or not.
# if not then it will calculate the inversion of the given matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInversion(m)
  m
}
