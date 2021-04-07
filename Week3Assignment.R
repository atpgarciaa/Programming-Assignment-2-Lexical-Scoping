makeCacheMatrix <- function(k = matrix()) 
{
  inv <- NULL
  set <- function(n){
    k <<- n
    inv <<- NULL
    
  }
 
  get <- function() {k}
  setInverse <- function(inversematrix) inv <<- inversematrix
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



cacheSolve <- function(k, ...) 
  
{
  inv <- k$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)    
  }
  matrix <- k$get()
  inv <- solve(matrix, ...)
  k$setInverse(inv)
  inv   
}
