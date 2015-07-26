## Create a special matrix to store cached inverse solution


## makeCacheMatrix generates a special matrix to hold cached inverse solution
## and functions to set the matrix and calculate the inverse.

makeCacheMatrix <- function(M = matrix()) {
  I <- NULL
  set <- function(x) {
    M <<- x
    I <<- NULL
  }
  get <- function() return(M)
  setInv <- function(inv) I <<- inv
  getInv <- function() return(I)
}


## Simple function to calculate inverse solution to the matrix

cacheSolve <- function(M, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- M$getInv()
  if(!is.null(I)){
    message("Retrieving cached solution")
    return(I)
  }
  m <- M$get()
  I <- solve(m)
  M$setInv(I)
  return(I)
}
