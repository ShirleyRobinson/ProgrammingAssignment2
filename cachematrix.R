## Put comments here that give an overall description of what your
## functions do
## Creating a special matrix to store a matrix and cache its inverse
## Write a short comment describing this function
## The functions create a special matrix that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y) {
    x<<- y
    inv<<- NULL
  }
    get<- function() x
    setInverse<- function(solveMatrix) inv<<- solveMatrix
    getInverse<- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by the makeCacheMatrix in the function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$setInverse(inv)
  inv
}
