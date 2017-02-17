# Author : SSGG
# Assignment 2
# Function Name : makeCacheMatrix
# Functionality : Cache the Inverse of a Matrix
# This function creates a matrix object and caches its own inverse.

makeCacheMatrix <- function(mt = matrix()) 
  {
    inv <- NULL
    set <- function(y) 
      {
      mt <<- y
      inv <<- NULL
      }
  get <- function() mt
  setInverse <- function(inverse) 
  inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }



# Function Name : cacheSolve
# Functionality : Compute the inverse of the matrix
# If the inverse has already been calculated it should retrieves the inverse from the cache.

cacheSolve <- function(mt, ...) 
  {
  inv <- mt$getInverse()
  if (!is.null(inv)) 
    {
    inv
    }
  else
    {
    mtx <- mt$get()
    inv <- solve(mtx, ...)
    mt$setInverse(inv)
    inv
    }
  }

#Example
#a <- diag(2,4)
#cmtx <- makeCacheMatrix(a)
#ia <- cacheSolve(cmtx)