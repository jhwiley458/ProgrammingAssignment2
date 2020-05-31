## Functions will calculate matrix inverse if it doesn't exist
## And cache it so it doesn't need to be recalculated 

## creates matrix with get and set functions

## usage j <- makeCacheMatrix(x = matrix(1:4, 2, 2))

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) x <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Returns inverse of matrix assuming non-singular 
## caches if hasn't already been calculated

## usage cacheSolve(j)

cacheSolve <- function(x, ...) 
{
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
