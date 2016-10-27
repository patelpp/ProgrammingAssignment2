## Overall, there are two functions that will calculate the inverse matrix or if the inverse matrix is already calculated
## the inverse matrix is retrieved from the cache

## The makeCacheMatrix creates the special matrix that contains 4 functions
## 1. get function returns the x vector
## 2. set function changes the x vector stored in the main function
## 3. setsolve function stores the solve function
## 4. getsolve function stores the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function calculates the inverse matrix if the inverse is not calculated, 
## if the inverse is calculated the the function gets the data stored in cache
 
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}