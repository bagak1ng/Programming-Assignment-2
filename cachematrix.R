makeCacheMatrix <- function(x = matrix()) {
b<-NULL
  set<-function(y){
  x<<-y
  b<<-NULL
}
get<-function() x
setI<-function(solve) b<<- solve #inverse matrix for calculating
getI<-function() b
list(set=set, get=get,
   setI=setI,
   getI=getI)
}

cacheSolve <- function(x, ...) {
     b<-x$getI()
    if(!is.null(b)){
      message("getting cached inv matrix")
      return(b)
    }
    m<-x$get()
    a<-solve(m, ...)
    x$setI(a)
    a
}
