## The following code provides a function which computes the inverse of matrices. If the inverse of a matrix has already been computed, 
## then cached information is used.  

## This function creates a list with four component functions. These functions set a current matrix, retrieve the current matrix, 
## sets the inverse of a current matrix, and computes the inverse of a current matrix

makeCacheMatrix <- function(x = matrix()) {
  ##inverse is set to blank matrix until computed
  inverse <- matrix()
  
  
  ##This function sets the matrix to a value
  
  set <- function(y){
    ## <<- used to set x which is in the scope of parent functions equal to input y 
    x <<- y
    ## set inverse to be NULL still as it has not been computed
    inverse <<- NULL
  }
  
  ##return matrix
  get <- function(){
    x
  }
  
  ##set inverse, input is a matrix
  setinverse <- function(y){
   inverse <<- y  
  }
  
  ##returns inverse
  getinverse <- function(){
    inverse
  }
  
  ##return list containing this matrix's information
  list(set=set, get=get, setinverse=setinverse, getinverse= getinverse)
  
}


##Find the inverse of a matrix if it is not cached. If the inverse is cached, return the inverse. Input to this function is a four parameter list. 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  ##check if matrix is set, if not, first element will be NA
  if(!is.na(inverse[1,1])){
    message("getting cached inverse matrix")
    return(inverse)
  }else{
    message("calculating inverse of matrix")
    ##get matrix
   m <- x$get() 
   ##calculate inverse
   inverse <- solve(m)

   ##set inverse of matrix as a caches value
   x$setinverse(inverse)
   return(inverse)
  }
  
  
}
