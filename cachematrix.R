## Matrix inversion can be a time consuming task. 
## That's whay this pair of functions can be used to perform this calculation,
## stores it in cache and use it without having to calculate it again. 
## The function 'makeCacheMatrix' will prepare the necessary data and internal
## functions (set, get, setsolve and getsolve) for 'cacheSolve', which will 
## return the inverse of the matrix, getting cached data or calculating it.




## This function takes a invertible matrix as an argument, saves it, and returns
## a list of functions that will be queried by the 'cacheSolve' function, to 
## calculate the inverse of the matrix.
## To simplify its use is recommended to save the result in an object like the
## example: m1 <- makeCacheMatrix(matrix(sample(36), nrow=6))

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y)
  {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function calculates the inverse of a matrix. 
## If that value has already been calculated the function returns it from the 
## cached data, with the message 'getting cache data'. 
## Instead, if it has not been previously calculated, this function calculates 
## the inverse of the matrix, stores it in cache for future uses and then 
## returns it. Example of use: cacheSolve(m1)

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s 
}
