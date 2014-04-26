## These functions allow for the caching of the inverse of a matrix
## for a matrix a, make the cached matrix ca<-makeCacheMatrix(a)
## cacheSolve(ca) returns the inverse the first time, the cached inverse from then on
## cacheSolve(ca) %*% a should return the identity matrix

## This function returns a list of functions that handle the caching of an inverse of a matrix
## if a is a matrix do ca<-makeCacheMatrix(a)
## ca$get() returns a
## if b is a matrix, ca$set(b) sets ca to the b matrix
## ca$getSolve() will return the inverse if it has been set, else null
## ca$setSolve(solve(b)) will set the inverse of the matrix b in a cache

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


## this function if passed a cacheMatrix object, will either return the cached inverse
## or calculate and cache and return the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
