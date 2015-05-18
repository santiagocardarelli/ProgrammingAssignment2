##The function makeCacheMatrix contains 4 different functions
## for managing the Matrix.
## The parameter for the function is:
## @x: a square invertible matrix (pre-requisite)
## return: a list containing functions to
##    1. set the matrix
##    2. get the matrix
##    3. set the inverse
##    4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  invmatrix<-NULL
  
  ##1. set the matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  ##2. get the matrix
  get<-function() x
  
  ##3. set the inverse
  setinv = function(invmatrix) inv <<- invmatrix
  
  ##4. get the inverse
  getinv = function() invmatrix
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##The function cacheSolve inverts the matrix checking if the matrix
## is not already inverted in the Cache.
## The parameter for the function is:
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  
  invmatrix = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(invmatrix)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(invmatrix)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  invmatrix = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(invmatrix)
  
  return(invmatrix)  
}