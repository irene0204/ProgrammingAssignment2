#The two functions below are used in conjuction to calculate/caches inverse of matrices

#The makeCacheMatrix function takes in an argument, which is the matrix whose inverse will be caculated.
#The default value is an empty matrix.
#It will return a list of 4 functions, set, get, setmean and getmean. Set function sets the new matrix
#to be calculated; Get function returns the matrix; Setmean function "saves" the inverse value; Get 
#function returns the inverse 

makeCacheMatrix<-function (x=matrix()){ 
  inv<-NULL 
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function ()x
  setinv<-function (inverse) inv<<-inverse 
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv) 
}



#cacheSolve will be used after makeCacheSolve function. 
#It takes in an argument x, which will be the list returned at the end of the makeCacheMatrix function 
#And then it sees if the matrix has already been calculated or not. If yes, it returns the cached value
#If not, it caculates the inverse and cache it 

cacheSolve<-function (x,...){
  #check to see if it is new matrix, if yes, calculate the inverse and chaces it.
  inv1<-x$getinv()
  if (!is.null(inv1)){
    message("getting cached data")
    return (inv1)
  }
  data<-x$get()
  inv1<-solve(data,...)
  x$setinv(inv1)
  inv1
}
