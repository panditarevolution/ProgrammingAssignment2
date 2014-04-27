## Put comments here that give an overall description of what your
## functions do



# This function creates a special matrix object that can cache 
# its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # setting inverse
  setinverse <- function(solve) m <<- solve
  # getting inverse
  getinverse <- function() m
  # return list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}




# This function computes the inverse of the special matrix 
# returned by makeCaheMatrix above. If the inverse has already
# been calculated (and the matrix not changed) then the 
# cachsolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  # check if inverse has already been calculatted
  if(!is.null(m)) {
    message("getting cached data")
    # return final value
    return(m)
  }
  ## calculate inverse
  # getting the matrix
  data <- x$get()
  # calculating inverse
  m <- solve(x, ...)
  # associating inverse with the special matrix
  x$setinverse(m)
  # returning the inverse
  m
}
