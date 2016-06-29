## Put comments here that give an overall description of what your
## functions do
## Return a matrix that is the inverse of 'x'

## create a square invertible matrix

## Write a short comment describing this function

## return a list which contains functions to:
## 1. set matrix
## 2. get matrix
## 3. set inverse of matrix
## 4. get inverse of matrix

## returned list is the input of cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  setM <- function(y) {
    x <<- y
    invM <<- NULL
  }
  getM <- function() x
  setinvM <- function(invVal) invM <<- invVal
  getinvM <- function() invM
  list(set=setM, get=getM, setinv=setinvM, getinv=getinvM)

}


## Write a short comment describing this function

## inverse of a returned square invertible matrix
## from makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getinv()
  if (!is.null(invM)) {
    message("cached matrix already exists !");
    message("inverse of matrix obtained !");
    return(invM) # return() is required
  }
  
  mat_data <- x$get()
  message("cached matrix just created !");
  invM <- solve(mat_data)
  message("inverse of matrix obtained !");
  ## save inverse of matrix for the future access
  x$setinv(invM) 
  invM
  
}
