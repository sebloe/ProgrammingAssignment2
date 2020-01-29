## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## defines matrix which can store the cached inverse matrix

makeCacheMatrix <- function(ma = matrix()) {
  i <- NULL
  set <- function(y) {
    ma <<- y
    i <<- NULL
  }
  get <- function() ma
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## First checks if cached value of  Matrix already exists
  ## if so, the cached value is returned
## if cached value does not exist the function use solve() function to build inverse matrix
## inverse matrix is then saved to cache via setInverse

cacheSolve <- function(ma, ...){
  i <- ma$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- ma$get()
  i <- solve(data, ...)
  ma$setinverse(i)
  i
}