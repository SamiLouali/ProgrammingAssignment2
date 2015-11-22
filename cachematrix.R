# Create the CacheMatrix object 

makeCacheMatrix <- function(x = matrix()) {
  CacheInverse <- NULL
  set <- function(y) {
    x <<- y
    CacheInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) CacheInverse <<- inverse
  getInverse <- function() CacheInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of the cacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Minverse <- x$getInverse()
  if(!is.null(Minverse)) {
    message("getting cached data")
    return(Minverse)
  }
  data <- x$get()
  Minverse <- solve(data, ...)
  x$setInverse(Minverse)
  Minverse
}


M <- matrix(1:4, nrow=2, ncol=2)
cacheMatrix <- makeCacheMatrix(M)
cacheSolve(cacheMatrix)