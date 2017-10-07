## These two function will create an inverse of a matrix and cache the 
## solution to the system memory. This is desireable because creating 
## the inverse of a matrix uses a lot of system resources. 

## This first function has four parts:
# 1. set the matrix value
# 2. get the matrix value
# 3. set the inverse matrix value
# 4. get the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This second function checks to see if there is a value for the 
## inverse of the matrix. If the matrix has been cached and stored in cache
## it will return the matrix and print a message of "getting cached matrix". 


cacheSolve <- function(x, ...)  {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached Matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}



## Sample Run
## matrix1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## matrix2 <- makeCacheMatrix(m1)
## matrix2$get()
##       [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75

## Sample cacheSolve first time
## cacheSolve(matrix2)
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4

## Sample cacheSolve second time
## cacheSolve(matrix2)
## getting cached Matrix
##      [,1] [,2]
## [1,]    6    8
## [2,]    2    4
