library(MASS)
makecachematrix <- function(x=matrix()) {
  inv <- NULL     #initializing inverse as NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x     #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x       #function to obtain inverse of the matrix
  }
  list(set=set,get = get,
       setinv = setinv,
       getinv = getinv)
}
## write a short comment describing this function
## this is used to get a cache data

cachesolve <- function(x, ...) ## gets cache data
{
  inv<-x$getinverse()

  if(!is.null(inv)){           #checking whether inverse is NULL
    message("getting cached data!")
    return (inv)        #returns inverse value
  }
  
  data<-x$get()
  inv<-solve(data,...)                 #calculates inverse value
  x$setinv(inv)
  inv    ##return a matrix that is the inverse of 'x'
  
}

