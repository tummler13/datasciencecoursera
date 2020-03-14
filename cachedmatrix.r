## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  #setting value to the matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
    getMatrix <- function() x                              #get the value of the Matrix
    setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
    getInverse <- function() invMatrix                     #get the value of the invertible matrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
    message("Getting Cached Inv. Matrix")  
    return(invMatrix)                             #return the invertible matrix
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     
  invMatrix <- solve(MatrixData, ...)             
  x$setInverse(invMatrix)                         #set the invertible matrix 
  return(invMatrix)                               
  ## Return a matrix that is the inverse of 'x'
}