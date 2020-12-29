## Matrix function
## Inverse of the matrix function

makeCacheMatrix <- function(x = matrix()) {
i <- NULL ## i is inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## return inverse of x

## Creating a cache of the function x

cacheSolve <- function(x, ...) {  ## given
        
        i <- x$getinverse()   ##inverse function
  if (!is.null(i)) {
    message("getting cached data")  ## message for cached
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
        ## Return i, that is a matrix (that is the) inverse of 'x'

