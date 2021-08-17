## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL ## Clear previous value of inverse
  }
  get <- function() x ## assign the matrix to get
  setInv <- function(solve) m <<- solve ## set the value of setInv to m (determined by solve, which calculates the inverse of the matrix)
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {  
  m <- x$getInv() ##see if there already is an inverse calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) ## assign the inverse of the matrix to m
  x$setInv(m) ## assign m to setInv
  m
}
        ## Return a matrix that is the inverse of 'x'
