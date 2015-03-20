## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a matrix object that
## is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseHolder <- NULL ## inverseHolder is initialized as NULL
  set <- function(y) {
    x <<- y
    inverseHolder <<- NULL
  } 
  get <- function() x
  setInverse <- function(inverse) inverseHolder <<- inverse
  getInverse <- function() inverseHolder
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ## The list of elements is returned
  
}


## The cacheSolve function computes the inverse of the matrix
## object from makeCacheMatrix or retrieves it from cache if
## that computation has already been completed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseHolder <- x$getInverse()
  if(!is.null(inverseHolder)) {
    message("getting cached data")
    return(inverseHolder)
  } ## If the inverseHolder is already populated with data, read from cache
  data <- x$get()
  inverseHolder <- solve(data)
  x$setInverse(inverseHolder)
  inverseHolder ## return the value of the inverseHolder
  
}
