# Author: Patrick Ericx
# Creation date: 22 june 2015
# Credentials: Guangming Lang on http://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/
#              I give him the credentials because of the very detailed explanation and the test strategy he had put on his blog.
# ------------------------------------------------------------------------------------------------------------------------------

## Overall description of this file: Cache a matrix inversion in R. 
##                                   We accomplish this with two functions: makeCacheMatrix, cacheSolve. 
##                                   Testing is done with one extra function: test, and 4 statements on the command line to initialize and run the test().

## Write a short comment describing this function
## makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
 


## Write a short comment describing this function
## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). If the inverse has already been calculated and the matrix has not changed, it’ll retrieves the ## inverse from the cache directly.

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse with the special function solve()
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  # Return a matrix that is the inverse of 'x'
  return(inv)
}

        
## Write a short comment describing this function
# test(): this function calculates 2x the inverse of a matrix. The first time it is calculated, and takes a lot of time, the second time it is fetched from the cache.
#         the difference between these two calculations should be significant.
		
test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}


# execute this test at the console to verify the output
# > set.seed(1110201)
# > r = rnorm(1000000)
# > mat1 = matrix(r, nrow=1000, ncol=1000)
# > test(mat1)
#
#
#
# output varies depending on the specific system on which you run it, but 
# the results should be similar
# 
# Time difference of 2.813229 secs
# getting cached data
# Time difference of 0.0009989738 secs		
