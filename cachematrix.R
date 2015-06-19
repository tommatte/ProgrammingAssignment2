## The following functions are used to calculate and cache the 
## inverse of a matrix. If the inverse has already been determined,
## then it is recovered from the cache without any computation. 
## Otherwise, the inverse is calculated and set in the cache.

## makeCacheMatrix is a function that receives as input a square 
## invertible matrix, and that gives as output a list containing 
## functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## cacheSolve is a function that receives as input the output
## of makeCacheMatrix and gives as output the inverse of the 
## original matrix given as input of makeCacheMatrix. 
## However, it first verifies if the inverse has already been 
## determined. In this case, it gets the inverse from the cache 
## without any computation. Otherwise, it determines the inverse
## and sets it in the cache using the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)){
        message("getting cached data")
        return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
