## In this program, I create two functions, which will provide functionality for matrix inversion, allowing 
## for saving previous inversions into the cache. Therefore, whenever a matrix inversion is calculated, if the
## user tries to invert the same matrix again, the new function will return the saved inverse matrix, instead of 
## going through all the computations again. 

## The function below allows for the creation of a special matrix, which is really a list containing a function to: 
## set and get the value of the matrix and set and get the value of the inverse matrix. This function will be used
## together with the next function cacheSolve, defined below. 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       getsolve = getsolve,
       setsolve = setsolve)    
}

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, 
## it calculates the inverse, using the command solve(), and sets the value of the inverse matrix 
## in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached inverse matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

## The code below was provide to test the built functions for matrix inversion

a <- matrix(c(10, 0, 0, 0, 20, 0, 0, 0, 30), nrow = 3, ncol = 3)
m <- makeCacheMatrix(a)
m$get()

## The first call to the function will actually execute the matrix inversion and save the result into the cache

cacheSolve(m)

## the second call will just retrieve the inverse matrix from the cache and return it

cacheSolve(m)


