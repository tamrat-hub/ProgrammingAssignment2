## First establish the matrix function
## Formulate the inverse of the matrix
## will get the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
i <- NULL                ## i is inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Take the above function and create a cache

cacheSolve <- function(x, ...) {  ## given
        
        i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
        ## Return i, that is a matrix (that is the) inverse of 'x'

