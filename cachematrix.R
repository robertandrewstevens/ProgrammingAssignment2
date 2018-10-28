# cachematrix.R
# JHU DS R Programming Week 3 
# Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
# 2018-10-26

# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(
    set = set, 
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}

cacheSolve <- function(x, ...) {
  # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve should retrieve the inverse from the cache.
  # Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

# Test the functions

set.seed(123)
m1 <- matrix(runif(5*5), 5, 5) # test matrix 1
sm1 <- makeCacheMatrix(m1) # special test matrix 1
cacheSolve(sm1)
cacheSolve(sm1)

set.seed(123)
m2 <- matrix(runif(500*5000), 500, 500) # test matrix 2
sm2 <- makeCacheMatrix(m2) # special test matrix 2
cacheSolve(sm2)
cacheSolve(sm2)
