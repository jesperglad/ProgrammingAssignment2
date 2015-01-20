## Creates a matrix that can holds a cache of the inverse matrix, including 
## functions for setting and returning the original value as well as the 
## inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Sets the cached matrix to NULL
  m <- NULL
  
  ## Defines the function set which stores the values of the matrix, and sets the inverse matrix to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Internal function returning the values of the matrix
  get <- function() x
  
  ## Internal function setting the inverse matrix
  setInv <- function(solve) m <<- solve
  
  ## Internal function returning the inverse matrix
  getInv <- function() m
  
  ## Returns list of functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Returns the catched inverse matrix given it is avaliable, 
## If the inverse matrix is not already catched then calculated it,
## catched it, and return it. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    
    ## Verify if the inverse matrix is already avalible
    if(!is.null(m)) {
      ## The inverse matrix is avalible, write message and return the inverse matrix
      message("getting cached data")
      return(m)
    }
    
    ## The inverse matrix is not avalible
    ## Get the values of the matrix
    data <- x$get()
    
    ## Calculate the inverse matrix
    m <- solve(data, ...)
    
    ## Catch the inverse matrix 
    x$setInv(m)
    
    ## Return the invese matrix
    m
}