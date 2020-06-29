## This function will cache the inverse of the matrix object. 

makeCacheMatrix <- function(x = matrix()) {
  # holds the cached value or NULL if nothing is cached
  # sets it to NULL when nothing is cached 
  m <- NULL
  # matrix is set 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # returns the set matrix 
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m 
  # returns a list of vectors 
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates the inverse of the matrix above (makeCacheMatrix) and if it remains unchanged, the inverse will be taken from the cache. 

cacheSolve <- function(x, ...) {
  # returns the cache value
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # inverse is stored in the cache 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

## Inverse of the "x" matrix 

> x <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
> cacheSolve(x)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
