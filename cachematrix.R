makeCacheMatrix <- function( m=matrix() ){
  
  x <-NULL
  set <- function(matrix) {
    m <<- matrix
    x <<- NULL
  }
  get <- function() {
    x
  }
  setInv <- function(inverse) {
    i <<- inverse
  }
  getInv <- function() {
    x
  }
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

######
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...) %*% data #matrix multiplication for inverse
  x$setInv(m)
  m
}