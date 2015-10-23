rm(list = ls())
#======================================================================
# makeCacheMatrix function creates a special square invertible "matrix" 
# object  that can cache its inverse. 
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse
# 4. get the inverse 
#======================================================================
makeCacheMatrix <- function(x = matrix()) 
{
  Invrse <- NULL
  # The <<- operator is used to assign a value to an object 
  # in an environment that is different from the current 
  set <- function(y) 
      {
         x <<- y
         Invrse <<- NULL
      }
  get <- function() x
  setinv <- function(inverse) Invrse <<- inverse 
  getinv <- function() Invrse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#======================================================================
# cacheSolve function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the 
# cachesolve retrieves the inverse from the cache
#======================================================================
cacheSolve <- function(x, ...) 
{
  Invrse <- x$getinv()
		    
  # Check if inverse already exists in cache. If yes, then get it and return
  if (!is.null(Invrse))
  {
     message("getting cached inverse")
     return(Invrse)
  }
				      
  # If inverse does not exist in cache, then use "solve" to find it. 
  Mdata <- x$get()
  Invrse <- solve(Mdata, ...)
						      
  # Set the inverse in the cache.
  x$setinv(Invrse)
  # cat ("Invrse =",Invrse,"\n")
  return(Invrse)
}

