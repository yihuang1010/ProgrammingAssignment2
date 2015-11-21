## write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInver <- function(solve) m <<-solve
  getInver <- function() m
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <-x$getInver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
  data <-x$get()
  m <- solve(data)
  x$setInver(m)
  m  ## Return a matrix that is the inverse of 'x'
}
