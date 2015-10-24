#makeCacheMatrix: This function creates a special "matrix" object 
#                 that can cache the inverse of it
makeCacheMatrix<-function(x=matrix()){
  #defined the variable matrx here  
  matrx <- NULL
  #set function is used to set the values of x
  set <- function(n) {
    x <<- n
    matr <<- NULL
  }
  #get function is used to search for the values of x
  get <- function() x
  #setinverse function is used to set the values of the inverse matrix
  setinverse <- function(solve) matrx <<- solve
  #getinverse function is to get the values of the inverse matrix
  getinverse <- function() matrx
  #list function is used to list down the existing sub-functions under the 
  #'makeCacheMatrix' function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  #get the inverse matrix that calculated previously
  matrx <- x$getinverse()
  #check for the successfulness of the previous calculation
  #if it is not a null value, it will skip the computational steps below 
  #and return the values for matr
  if(!is.null(matrx)) {
    message("getting cached data")
    return(matrx)
  }
  #if the condition is false, continue the computational steps as shown below
  data <- x$get()
  matrx <- solve(data, ...)
  x$setinverse(matrx)
  matrx
}