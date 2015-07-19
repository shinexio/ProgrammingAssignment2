##1.makeCacheMatrix(): creates a “matrix” can cache its inverse.

##2.cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix().

## x is a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##    this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(solve) inv<<- solve
  getinv<-function() inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## x is the output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x=matrix(), ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  inv<-x$get()
  inv<-solve(inv, ...)
  x$setmatrix(inv)
  inv
}

## test the functions
C <- sample(1:10,100,replace=TRUE)
Cma <- matrix(C, nrow=10,ncol=10)
cacheSolve(makeCacheMatrix(Cma))
