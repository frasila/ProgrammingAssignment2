## The two functions below follow the template of the example provided in the
## programming assignment

#makeCacheMatrix creates the same "vector" object as the assignment's example:
#it contains a list with a funtction to

#1 set the value of the matrix
#2 get the value of the matrix
#3 set the value of the inverse
#4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #1 set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #2 get the value of the vector
  get <- function() x
  # 3 set the value of the mean
  setinverse <- function(solve) m <<- solve
  # 4 get the value of the mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve calculates the inverse of the "vector" created in makeCacheMatrix.
#It first checks if the inverse has already been calculated, then either gets
#the cached inverse or computes the inverse and sets the cached inverse with the
#setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



#Test the functions with this example (copy in Console):
#A <- matrix(c(2,1,5,3), nrow = 2,ncol = 2)
#b <- makeCacheMatrix(A)
#cacheSolve(b)