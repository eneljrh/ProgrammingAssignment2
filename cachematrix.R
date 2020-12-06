## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## initialize
  
   inverse <- NULL
  
  set <- function(y) {
    ## assign x value 1
    x <<- y
    ## assign null inverse
    inverse <<- NULL
    
  }
  
  ## define function get
  get <- function() x
  
  ## define function set inverse
  setinverse <- function(inver) inverse  <<- inver
  
  ## define function get inverse
  getinverse <- function() inverse
  
  ## return the element list
  list(set = set, get = get,
       
       setinverse = setinverse ,
       
       getinverse = getinverse )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## obtain the inverse value
  inverse  <- x$getinverse()
  
  ## if it exist we can return it and we dont need to calculate it
  if(!is.null(inverse)) {
    
    message("getting cached data")
    
    return(inverse)
    
  }
  
  data <- x$get()
  
  ## we calculate the inverse
  inverse <- solve(data)
  
  ## we actualize the object list with the inverse value
  x$setinverse(inverse)
  
  ## return inverse
  inverse
}
