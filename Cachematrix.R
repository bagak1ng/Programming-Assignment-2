makeCacheMatrix <- function(j = matrix()) {
  b<-NULL
  s<-function(y){
    j<<-k
    b<<-NULL
}
  g<-function() j
  s1<-function(sol) b<<- sol
  g1<-function() b
  list(s=s, g=g, s1=s1, g1=g1)
}

#checked inverse matrix

cacheSolve <- function(j, ...) {
  b<-j$g1()
  if(!is.null(b)){
    message("get matrix all")
    return(b)
  }
  m<-j$g()
  b<-sol(m, ...)
  x$s1(b)
  b
}
