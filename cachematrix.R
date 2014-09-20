## These two functions are used to calculate the inverse of a matrix
## if the matrix has already been solved, it will get a cached version

## Creates a list of matrices where the inverse has been calculated, 
## and retreieves this value. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


## This function checks if a matrix inverse has been calculated and if so 
## retreives from the cache, if it has not been calculated it will do so

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
