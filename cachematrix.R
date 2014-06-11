##The below contains two functions: 
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



## create a special vector containing functions to
## 1 sets the values of the matrix 
## 2 gets the values of the matrix
## 3 sets the values of the matrix inverse
## 4 gets the value sof the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  
  #set initial value of inverse to Null
  inv <- NULL               
  
  #if the set function is called update the values of x
  #and clear the cached matrix inverse
  set <- function(y) {
    x <<- y                 
    inv <<- NULL            
  }
  
  #function returns the matrix values
  get <- function() x       
  
  #caches matrix inverse based on input
  setInv <- function(newInv) inv <<- newInv  
  
  #returns cached value of the matrix inverse
  getInv <- function() inv
  
  #compile individual functions into list to return
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve calculates the matrix inverse of the special vector 
## created with the above function and caches it. It first checks to see
## if the inverse already exists, if it does than it returns the inverse
## that was already calculated. Otherwise it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setInv function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #check to see if inverse has already been calculated with given matrix x
  inv <- x$getInv()
  
  #if the inverse already exists return cached value and display message
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  
  #if inverse was not stored previously retrieve matrix data
  data <- x$get()
  
  #calculate matrix inverse
  inv <- solve(data, ...)
  
  #cache matrix inverse 
  x$setInv(inv)
  
  #return resulting matrix inverse
  inv
}
