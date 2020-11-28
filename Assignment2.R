x <- c(1,1,1,1)
dim(x)<-c(2,2)

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setsolve <- function(solve) m <<- solve
getsolve <- function() m
list(set = set, get = get,
     setsolve = setsolve,
     getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

m <- matrix(rnorm(16),4,4)
makeCacheMatrix(m)
cacheSolve(makeCacheMatrix(m))
