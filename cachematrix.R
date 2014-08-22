## The following are a pair of functions that cache and recall
## the inverse of a matrix

## This function creates a special "matrix" object
## that can cache its inverse


makeCacheMatrix <- function(x = matrix())   
{
  s <- NULL
  
  # Set value of set to define matrix
  set <- function(y) 
  {
    x <<- y
    s <<- NULL
  }
  
  # Set value of get to matrix value
  get <- function() 
  {
    x
  }
  
  # Set Value of setSolve
  setSolve <- function(solve) 
  {
    s <<- solve
  }
  
  # Set value of getSolve
  getSolve <- function() 
  {
    s
  }
  
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## This function to get the inversed matrix from a special object created by makeCacheMatrix.
## Takes the object of that type as an argument 'x', checks if the inverse value is already
## cached, and if it is returns the cached value; if not, this function calculates the
## inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setSolve'
## and returns the result.

cacheSolve <- function(x, ...) {
  
  # Checking to see if matrix getSolve value is cached
  s <- x$getSolve()
  if(!is.null(s)) 
  {
    message("getting cached data")
    return(s)
  }
  
  # If data is not already cached, create the data and return
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}