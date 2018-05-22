#The makeCacheMatrix function takes in an argument, which is the matrix whose inverse will be caculated.
#The default value is an empty matrix.
#It will return a list of 4 functions, set, get, setmean and getmean. Set function sets the new matrix
#to be calculated; Get function returns the matrix; Setmean function "saves" the inverse value; Get 
#function returns the inverse 

makeCacheMatrix<-function (x=matrix()){ 
  inv<-NULL 
  #"inv" is the variable that takes the inverse value 
  #when a new matrix to be calculated is put in the argument, the corresponding "inv" will be 
  #set to be empty (NULL), so that the cacheSolve function can recpgnize it's a new matrix and do its 
  #job
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  #the set function does 2 things:
  #First, takes in an argument y, and assign it to x, which is in the parent environment (<<- is used)
  #in this assignment y will be a new matrix to be calculated. By doing this, x will be set to be the 
  #new matrix throughout the function 
  #Second, when the user use the set function to set the new matrix to be calculated, inv is set to be
  #empty (NULL)
  get<-function ()x
  #the get function simply returns the matrix 
  setinv<-function (inverse) inv<<-inverse 
  #the setinv function takes in an argument "inverse" and assign it to inv, which is in the parent 
  #environment. In this assigment the argument will be the inverse of the matrix, which will be calculated with
  #the cacheSolve function. This way the inverse of the matrix is "saved" in the varaible "inv" and can 
  #be retrieved later
  getinv<-function() inv
  #the getinv function return inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  #finally the function makeCacheMatrix return a list of 4 functions 
}



#cacheSolve will be used after makeCacheSolve function. 
#It takes in an argument x, which will be the list returned at the end of the makeCacheMatrix function 
#And then it sees if the matrix has already been calculated or not. If yes, it returns the cached value
#If not, it caculates the inverse and cache it 

cacheSolve<-function (x,...){
  inv1<-x$getinv()
  #assign inv in the makeCacheMatrix function to inv1
  if (!is.null(inv1)){
    message("getting cached data")
    return (inv1)
  }
  #if inv1 is not empty, then that means the inverse of this matrix has already been calculated
  #a message saying "getting cached data" will be returned and the cached value is returned
  data<-x$get()
  #else, the new matrix is retrieved using the get function and will be assigned to data
  inv1<-solve(data,...)
  # the inverse is calculated and assigned to inv1
  x$setinv(inv1)
  #assign inv1 to inv in the makeCacheSolve function, the inverse value is cached
  inv1
  #return inv1 
}
