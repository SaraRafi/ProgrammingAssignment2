## the makeCacheMatrix function creates a matrix object that has the ability to cache its inverse.
## the cacheSolve function computes the inverse of a matrix, if it doesn't already exist, and returns the cached version of the inverse, if one exists.

## this function creates a matrix whose inverse can be cached. it creates a list of set and get functions for the matrix and its inverse.
makeCacheMatrix <- function(x = matrix())  
{
  Inverse <- NULL
  
  set <- function(y) 
  {
    if (is.matrix(y))
    {
      x <<- y
      Inverse <<- NULL
    }
    else
    {
      message("Input is not a valid matrix.")
    }
  }

  get <- function()
  {
    x
  }
  
  SetInverse <- function(Input_Inverse)
  {
    if (is.matrix(Input_Inverse))
    {
      Inverse <<- Input_Inverse
    }
  }
  
  GetInverse <- function()
  {
    Inverse
  }
  
  list(set = set,
       get = get,
       SetInverse = SetInverse,
       GetInverse = GetInverse)
}

# This function can be called on a matrix object created using the makeCacheMatrix function. it returns the cached inverse if it exists. if a cached inverse doesn't exist, it computes the matrix inverse and saves it on the cacheMatrix object.
cacheSolve <- function(x, ...) {
  inverse <- x$GetInverse()
    
  if(!is.null(inverse)) {
    message("getting cached Matrix Inverse")
    print(inverse)
    return(inverse)
  }
    
  message("Calculating Inverse")
  input_matrix <- x$get()
  inverse <- solve(input_matrix)
  x$SetInverse(inverse)
  print(inverse)
  inverse
}
  
M <- matrix(c(2,3,2,2), 2, 2)
A <- makeCacheMatrix(M)
inv_A <- cacheSolve(A)
inv_A <- cacheSolve(A)
  
