## These functions cache the inverse of a matrix by calculating it and storing the value
## in the object when the inversion is requested for the first time.
## The cached value will be returned when the matrix inversion is be requested again

## This function creates a special "matrix" object that can cache its inverse.
## It a list containing a function to set and get an initial matrix and set and get it's inversion

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  
  get <- function() {
    x
  }  
  
  setinverse <- function(inverse){
    inv <<- inverse
  }
  
  getinverse <- function(){
    inv
  } 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates and returns the inversion of the initial matrix
## It checks if the inversion has already been calculated and depending on the 
## check return the value or calculates the inversion, stores it in the object's cache 
## ia the setinverse function and returns the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
