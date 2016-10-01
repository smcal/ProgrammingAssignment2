## Matrix Inversion - These pair of objects calculate the 
## inverse of a matrix.  The makeCacheMatrix function
## encapsulates the ability to create a matrix and to 
## cache the inverse of the matrix elliminating the need
## to calculate the inverse every time it is requested
##
## The cachSolve will do the actual calculation to inverse
## the matrix.  In will first check if the inverse is already
## calculated and only if it is not will it proceed with the acutal
## calculation.
##
## This algorithm limits the number of times the inverse calculation
## takes place to only one 


## makeCacheMatrix - This object creates the matrix and can store the 
## inverse of the matrix for later retrieval.  This function assumes
## the passed in matrix is square

makeCacheMatrix <- function(inputMatrix = matrix()) 
{
  matrixInverse <- NULL
  
  ## Store the passed in matrix and clear the inverse variable
  setMatrix <- function(y)
  {
    inputMatrix <<- y
    matrixInverse <<- NULL
  }
  
  ## Retrieve the passed in matrix
  getMatrix <- function() inputMatrix
  
  ## Store the calculated inverse of the matrix
  setMatrixInverse <- function(inverse) matrixInverse <<- inverse
  
  ## Retrieve the inverse of the matrix
  getMatrixInverse <- function() matrixInverse
  
  ## Return a fully formed object of type makeCacheMatrix with each
  ## element being named allowing us to use the $ to extract the operators
  list( setMatrix = setMatrix,
        getMatrix = getMatrix,
        setMatrixInverse = setMatrixInverse,
        getMatrixInverse = getMatrixInverse )
}


## cacheSolve - This function calculates the inverse of a matrix
## that is of type makeCacheMatrix.  That is it expects as input a
## object of type makeCacheMatrix and it will use the setters and getters
## of that object to determine if it needs to calculate the inverse.  If the
## inverse is already calculated it just returns the already stored value 
## otherwise it will calculate the inverse and store it in the makeCacheMatrix object

cacheSolve <- function(makeCacheMatrixType, ...) 
{
  matrixInverse <- makeCacheMatrixType$getMatrixInverse()
  if( !is.null( matrixInverse ) )
  {
    ## Inverse is already calculaed so just return the value
    message("Retrieving cached value of the inverse of the matrix")
    return( matrixInverse )
  }
  
  ## If we are here then the inverse does not exist so go ahead and calculate
  ## and store the result
  origMatrix = makeCacheMatrixType$getMatrix()
  matrixInverse <- solve( origMatrix )
  makeCacheMatrixType$setMatrixInverse( matrixInverse )
  matrixInverse
}
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
