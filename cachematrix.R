## The two functions 'makeCacheMatrix' and 'cacheSolve' calculate the inverse of a matrix and
## cache it; it is assumed that the matrix that is passed in the function 'makeCahceMatrix' is
## invertibele, i.e. there is no mechanism to intercept an error in the case of a matrix that
## is not invertible


## The function 'makeCacheMatrix' takes an (invertible) matrix as argument and returns a list
## containing four elements. The four elements are functions that (1) set the value of the matrix,
## (2) get the value of the matrix, (3) set the inverse of the matrix, and (4) gets the 
## inverse of the matrix. The four functions are defined such that their returned values are 
## available in an environment that is different to the environment where they are defined

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # (1) set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # (2) get the value of the matrix
  get <- function() x
  # (3) calculates the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  # (4) gets the inverse of the matrix
  getinverse <- function() m
  # combine (1)-(4) in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve" calculates the inverse of the matrix that was passed to the function
## "makeCacheMatrix" (see above). It first checks if the inverse is already cached. If yes the 
## calculation is skipped and the inverse from the cache is returned (the message "getting cached 
## data" is displayed). If no, the matrix is gotten from the get-function, the inverse is 
## calculated using the solve-function, the inverse is cached (setinvers-function) and returned 
## eventually (in addition the message "calculate inverse for the first time" is displayed).
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # check if inverse already exists in cache, if yes, skip calculation and return inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if inverse does not exist, get the matrix and calculate, cache and return the inverse
  message("calculate inverse for the first time")
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
