##This function returns list which contains functions
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function
##This function will retun the inverse of matrix 'x'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
 ##Checking if the value already exists and then return it from cache
   if(!is.null(m)) {
    message("getting cached data")
    return(m)
   }
  ##If the value is not calculated this portion will calculate inverse and retun the value
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
