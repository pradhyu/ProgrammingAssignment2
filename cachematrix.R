## Functions for creating and using inverted matrices which caching ability

## makeCacheMatrix computes inverse of a matrix and stores it in memory
## cacheSolve shows the inverse of a matrix if is in memory or computes the 
## inverse and then shows the inverse


## makeCacheMatrix uses scoping rules and stores inverse of matrix in memory.
## When a newvalue is set to the matrix the inverse is set to NULL
## This function also checks if the input provided is matrix otherwise shows error 
## and stops processing
makeCacheMatrix <- function(matrix = matrix()) {
  # Let's check if we have correct input
  if (!is.matrix(matrix)) {
    stop("Please provide a matrix as parameter")
  }
  matrix.inverse <- NULL
  set <- function(newvalue){ 
    matrix <<-newvalue
    matrix.inverse <<- NULL
  }
  get <- function() matrix
  # getter and setters for matrix.inverse
  set.matrix.inverse <- function (inverse) matrix.inverse <<- inverse
  get.matrix.inverse <- function() matrix.inverse
  list(set=set, 
       get=get, 
       set.matrix.inverse= set.matrix.inverse,
       get.matrix.inverse=get.matrix.inverse)
}

## A function that shows the cache matrix.inverse when it's already been calculated and there's 
## no changein in the matrix
## otherwise computes it using R function solve and then caches it for future use.
## Assumption : For this assignment, assume that the matrix supplied is always invertible. 
##              i.e. the matrix  provide to cacheSolve will be square matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get.matrix.inverse()
  if(!is.null(inverse)){
    message("matrix inverse is found in the memory, skip computing and get it from cache")
    return(inverse)
  }
  message("inverse for this matrix not found in memory so the inverse will be computed and cached")
  data <- x$get()
  inverse <- solve(data, ...)
  x$set.matrix.inverse(inverse)
  inverse
}

# unit-tests
# square matrix : should coupute matrix inverse for first time cacheSolve is called 
##                From second call the inverse should be presented from memory/cache
X <- matrix(rpois(100,3), nrow = 10)
cX <- makeCacheMatrix(X)
cX$get()
cacheSolve(cX)
cacheSolve(cX)
invX <- cacheSolve(cX)

## if input is not matrix : show error asking user to privide matrix as parameter to function
Y <- 1
cY <- makeCacheMatrix(Y)
