## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix takes an invertible matrix and inverses the matrix setting the variable as 'I'. 
# It also creates and returns a list of functions that is used as the input of cacheSolve. 
# cacheSolved is then used to return the cached data. 
# If the cached data is null (unset) then the get function is called to perform the computation, set 'I'
# and return 'I'




## Write a short comment describing this function

## the input of makeCacheMatrix 'x' is an invertible matrix
## the return of makeCacheMatrix is a list of functions to set and get the matrix as well as set and get the inverse
## the list is used as the input to cacheSolve(). 


makeCacheMatrix <- function(x = matrix()) {
    
  I <- NULL
  set <- function(y) { 
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setI <- function(inverse) I <<- inverse 
  getI <- function() I
  list(set=set, get=get, setI=setI, getI=getI)

}


## Write a short comment describing this function

## x = the list of functions produced by makeCacheMatrix
## cacheSolve returns the inverse of x either from 'cache' or by performing the calculation

cacheSolve <- function(x, ...) {
 
  I = x$getI()
  
  # check if I is not null. If it isn't then I will contain the inverse
  if (!is.null(I)){
   
    message("getting cached data")
    return(I) # return the cached inverse matrix
  }else{
  
  # I must be null. Therefore the inverse has not been calculated or set  
    data <- x$get()
    I <- solve(data, ...)
  
    x$setI(I)
  
    return(I)
  }
  
  
}
