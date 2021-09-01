## It has two functions makeCacheMatrix 
## It will calculate inverse for non squared and squared matrices

 
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
 
  get<-function() x
  setInv<-function(inverse) inv<<-inverse
  getInv<-function() inv
  list(set=set,
       get=get,
       setInv=setInv,
       getInv=getInv)
}
 
## Getting cache data
## Also observing if inverse is null 

 
cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)) {
    message("retrieving matrix")
    return(inv)
  }
 
  ans<-x$get()
  inv<-solve(ans,...)
  x$setInv(inv)
  inv
 
}