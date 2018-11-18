## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() inv
  setInverse <- function(inverse) inv <<- inverse
  getmean <- function() inv
  list(set = set, 
       get = get,
       setInverse = setmean,
       getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- inv$get$Inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- inv$get()
  inv <- solve(mat, ...)
  inv$setInverse(inv)
  inv
}
