## This functions will compute matrix inverse and caching it in case that same inverse 
## could need it again.

## makeCacheMatrix: creates a "special matrix" and prepares to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #init the inverse...    
  inversa <- NULL
  
  #set the matrix and put to NULL the inverse. If the matrix changes, the inverse is computed again
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  #get the matrix...
  get<-function() x
  
  #cachin the inverse of the matrix
  setinv <- function(inve) inversa <<- inve
  #get the inverse (not compute :) )
  getinv <- function() inversa
  list(set=set, get=get,setinv=setinv,getinv=getinv)
       
}


## With a "special matrix", cacheSolve compute the inverse, if this inverse isn't been calculate, or 
## return the cached value, when exists. 

cacheSolve <- function(x, ...) {
        ## get the inverse and checks that value isn't NULL....
	      r_inve <-x$getinv()
        if (!is.null(r_inve)){
           message (" the inverse is...")
           return(r_inve) ## The inverse was cached
        }
        ##Get matrix...
        mt<-x$get()
        ##compute the inverse
        r_inve <- solve(mt)
        ##cached the value
        x$setinv(r_inve)
        r_inve ## ready... :)
}
