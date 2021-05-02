# makecacheMatrix basically store the inverse of a matrix. 

makecacheMatrix <- function(x_0 = matrix()) {
  inverted_0<- NULL
  set_0 <- function(m){
    x_0 <<- m
    inverted_0 <<- NULL
  }
  get <- function()x_0
  setinv <- function(i) inverted_0 <<- i
  getinv <- function() inverted_0 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# checking to see if inverse of matrix exists already in cache and if not present , calculate and store it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted_0 <- x_0$getinv()
  if(!is.null(inverted_0)){
    return(inverted_0)
  }
  mat <- x_0$get()
  inverted_0 <- solve(mat) 
  #since another argument is missing, solve gives the inversion of said matrix
  x_0$setinverse(inverted_0)
  inverted_0
}
