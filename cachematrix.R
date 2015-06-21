
## The function creates a cache for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## initialize a function that creates or updates the inverse matrix object
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  

}


## The function solves for the inverse of the matrix. It first checks if the Solution 
## exists in the cache. Else the inverse is computed.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        ## Returns a matrix that is the inverse of 'x'
}