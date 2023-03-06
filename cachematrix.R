## Two functions that cache the inverse of a matrix

## makeCacheMatrix creates the object 'matrix' to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## inverse property
  i <- NULL
  
  ## create matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }

  ## call matrix function
  get <- function() {
    ## return matrix
    m
  }

  ## set invertible matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }

  ## get invertible matrix
  getInverse <- function() {
    ## return inverse
    i
  }
  
  ## return methods list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve computes the inverse of the special matrix created by makeCacheMatrix
## if the inverse is already calculated, cacheSolve retrieves the inverse

cacheSolve <- function(x, ...) {

  ## return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## return inverse if already cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get matrix from object
  data <- x$get()
  
  ## calculate inverse with matrix multiplication
  m <- solve(data) %*% data
  
  ## set inverse to object
  x$setInverse(m)
  
  ## return matrix
  m
  
}