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

## Text
m<- c(1,1,0,1,0,1,0,1,0)
dim(m)<-c(3,3)
m

makeCacheMatrix(m)
cacheSolve(makeCacheMatrix(m))
