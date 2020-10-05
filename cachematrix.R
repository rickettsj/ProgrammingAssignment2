## These functions create a retrievable matrix and the inverse of that matrix.
## This function creates the object
makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) inverted <<- solve(x)
  getinvert <- function() inverted
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## This function creates the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getinver()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinvert(invert)
  invert
  }
