## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

## create a 'closure' with the property 'cachedInverse'

makeCachedMatrix <- function(m = matrix()) {
  cachedInverse <- NULL

  get <- function() m
  set <- function(y) {
    m <- y
    cachedInverse <- NULL
  }
  
  getCachedInverse <- function() cachedInverse
  setCachedInverse <- function(x) cachedInverse <<- x
  
  list( get = get,
        set = set,
        getCachedInverse = getCachedInverse,
        setCachedInverse = setCachedInverse )
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        
  inverse <- x$getCachedInverse()
  
  ## if the inverse exists, return the inverse
  
  if (!is.null(inverse)) {
    message("Found the Cached Inverse")
    return (inverse)
  }
  
  mat <- x$get()
  inverse <- solve(mat)
  x$setCachedInverse(inverse)

  inverse
}
