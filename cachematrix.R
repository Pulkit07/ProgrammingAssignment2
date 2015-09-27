## makeCacheMatrix contains four basic functions and cacheSolve is a function
## which checks is inv already exist in our memory returns otherwise compute store
##and return the inverse.

## Four functions for setting and returning the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv<-matrix(NA,nrow(x),ncol(x))
  set<-function(y=matrix()){
    x<<-y
    inv<<-matrix(NA,nrow(y),ncol(y))
  }
  get<-function() x
  setinv<-function(inver) inv<<-inver
  getinv<-function(inver) inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  bool<-FALSE
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      if(!is.na(x[i,j]))
        bool<-TRUE
    }
  }
  if(bool){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get
  inv<-solve(mat)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
