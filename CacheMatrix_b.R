#create special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


#Compute the inverse of special "matrix" returned by makeCacheMatrix.
#If inverse has already been calculated, retrieve inverse from cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

#test output

m <- makeCacheMatrix(matrix(2:5, 2, 2))
m$get()
#     [,1] [,2]
#[1,]    2    4
#[2,]    3    5

m$getinv()
#NULL

cacheSolve(m)
#     [,1] [,2]
#[1,] -2.5    2
#[2,]  1.5   -1

cacheSolve(m)
#Getting cached data
#     [,1] [,2]
#[1,] -2.5    2
#[2,]  1.5   -1

m$getinv()
#     [,1] [,2]
#[1,] -2.5    2
#[2,]  1.5   -1

m$set(matrix(9:6, 2, 2))
m$get()
#     [,1] [,2]
#[1,]    9    7
#[2,]    8    6

m$getinv()
#NULL

cacheSolve(m)
#[,1] [,2]
#[1,]   -3  3.5
#[2,]    4 -4.5

cacheSolve(m)
#Getting cached data
#     [,1] [,2]
#[1,]   -3  3.5
#[2,]    4 -4.5

m$getinv()
#     [,1] [,2]
#[1,]   -3  3.5
#[2,]    4 -4.5
