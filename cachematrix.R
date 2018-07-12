## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  invers <- matrix()
  set <- function(y) {
    x <<- y
    invers <<- matrix()
  }
  get <- function() x
  setinvers <- function(inv) invers <<- inv
  getinvers <- function() invers
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  invers <- x$getinvers()
  if(!is.na(invers)) {
    message("getting cached data")
    return(invers)
  }
  matrx <- x$get()
  invers <- solve(matrx, ...)
  x$setinvers(invers)
  invers

}
