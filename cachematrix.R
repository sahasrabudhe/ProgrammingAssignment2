## The below pair of functions cache the inverse of a matrix

## This function will creat a special Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function()
    x
  setInv <- function(inverse)
    m <<- inverse
  getInv <- function()
    m
  list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if (!is.null(m)) {
    message("getting cached Inverse")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
  
}
