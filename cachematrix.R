## R function to create matrix and then cache its inverse, if not done already. 
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL 
  }
  
  get <- function() x
  setinverse <- function(inverseMatrix) m <<- inverseMatrix
  getinverse <- function() m
  list(set = set, get = get, setinverseerse = setinverseerse, getinverse = getinverse)
  
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverseerse(m)
  m 
  ## Return a matrix that is the inverse of x
}
