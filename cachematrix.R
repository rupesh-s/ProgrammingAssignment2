## This file implements two functions as stated below
## makeCacheMatrix - Generates a "matrix object that can cache it's inverse
## cacheSolve - Computes the inverse of a matrix
## NOTE : Assuming that the matrix supplied is always invertible

## Function to create a matrix to cache it's  inverse
## input data type: matrix
## return data type : matrix
makeCacheMatrix <- function(x = matrix()) {
  ## initialize a cache to store the inverse of the matrix
  localCache <- NULL
  ## Setter function to create a matrix
  set <- function(y) {
    x <<- y
    localCache <<- NULL
  }
  ## getter function for the input matrix
  get <- function() x
  ## setter function for assigning values to the inverse of matrix 
  setInverse <- function(inversematrix) localCache <<- inversematrix
  ## getter function to retreve the value of the inverse of matrix
  getInverse <- function() localCache
  
  ## create a list of functions exposed
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function to generate the inverse of a matrix 
## input data type : matrix
## return data type : matrix
cacheSolve <- function(x, ...) {
  inversematrix <- x$getInverse()
  
  ## check if the inverse already exists
  if(!is.null(inversematrix)){
    return(inversematrix)
  }
  ## if not then generate inverse and "set" it
  inputdata <- x$get()
  inversematrix <- solve(inputdata, ...)
  x$setInverse(inversematrix)
  ## Return a matrix that is the inverse of 'x'
  inversematrix
}
