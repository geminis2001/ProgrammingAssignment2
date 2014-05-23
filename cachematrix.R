## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      
  inversa <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setinv <- function(inve) inversa <<- inve
  getinv <- function() inversa
  list(set=set, get=get,setinv=setinv,getinv=getinv)
       
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	      r_inve <-x$getinv()
        if (!is.null(r_inve)){
           message (" the inverse is...")
           return(r_inve)
        }
        mt<-x$get()
        r_inve <- solve(mt)
        x$setinv(r_inve)
        r_inve
}
