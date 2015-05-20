## We want to cache potentially time-consuming computations. 
## This cashes the inverse of a matrix. 


## This function creates a special "matrix" object that can 
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  myinverse<-NULL
  
  set<-function(y){
    x<<-y
    myinverse<<-NULL
  }
  
  get<-function(){
    x
  }
  
  setinverse<-function(inverse){
    myinverse<<-inverse
  }
  
  getinverse<-function(){
    myinverse
  }
  
  list(set=set, get=get, setinverse=setinverse, 
       getinverse=getinverse)
}


## This function computes the inverse of the special 
# matrix returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the 
# inverse from the cache. 
# I assume the matrix is always invertible. 
cacheSolve <- function(x, ...) {
  
  myinverse<-x$getinverse()
  
  if(is.null(myinverse)){
    mymatrix<-x$get()
    myinverse <- solve(mymatrix, ...)
    x$setinverse(myinverse)
  }
  else{
    message("getting cashed data")
  }
  
  myinverse
}
