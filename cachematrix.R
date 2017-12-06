## Functions that get the inverse of a matrix and caches it
## The function creates a matrix that stores the inverse after solving
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Starts inverse property
  i <- NULL
  
  ## Sets matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  ## Get matrix
  get <- function() {
    ## Returns matrix
    m
  }
  ## Set inverses matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## Get inverse of matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## lists executed functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Cachesolve computes the inverse of matrix, else retrieves it
cacheSolve <- function(x, ...) {
  
  ## Return a the inverted matrix x
  m <- x$getInverse()
  
  ## Returns inverse if already computed
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Gets matrix from the variable
  data <- x$get()
  
  ## Calculates inverse
  m <- solve(data) %*% data
  
  ## Set inverse
  x$setInverse(m)
  
  ## Return matrix
  m
}