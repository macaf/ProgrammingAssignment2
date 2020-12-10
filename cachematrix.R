
makeCacheMatrix <- function(x = matrix()) {
  #Make a list with a matrix, and the inverse if exist, if the inverse dont exist the value is null
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<-function(inv) m<<-inv
  getinverse<-function()m
  list(set = set, get = get,
       setinverse= setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...){
  #Calculate the inverse of the matrix, if getinverse is null, the argument is a list.
  #If setinverse is not null, return the argument of setinverse.
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
