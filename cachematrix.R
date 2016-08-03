## makeCacheMatrix creates a list containing a function which
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix. OBS: the matrix is always invertible.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
## check if the inverse has already been computed  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
## if not: compute the inverse and solve the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}

## TEST

## Case 1: new calculation

##matrix = rbind(c(1,3), c(2,4))
## > matrix
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > a = makeCacheMatrix(matrix)
## > cacheSolve(a)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## case 2: inverse already calculated
## > cacheSolve(a)
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5