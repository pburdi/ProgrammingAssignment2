## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function initializes and makes a cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmax<-function(solve) m<<- solve
  getmax<-function() m
  list(set=set, get=get,
       setmax=setmax,
       getmax=getmax)
}

## Write a short comment describing this function
## This function resolves the cached matrix and takes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmax()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmax(m)
  m
}
