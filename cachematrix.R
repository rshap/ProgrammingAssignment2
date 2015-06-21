## The following functions work together to cache the inverse of a matrix to 
## save time on a potentially time-consuming computation

## makeCacheMatrix creates a "matrix" (really a list) that works with the 
## cacheSolve function
## It keeps track off the matrix and its inverse (once computed)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve checks if the inverse of a "matrix" was already computed and if so
## returns it. If the inverse has not yet been computed, it computes the inverse,
## sets it so that it can be used again if necessary, and returns the newly 
## computed inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
