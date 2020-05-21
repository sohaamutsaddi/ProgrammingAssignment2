#The first function, makeVector creates a special "vector", which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invmat <- NULL
  
  #setFunction
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  #getFunction
  get <- function() x
  
  #setInverse Function
  setInverse <- function(inverse) invmat <<- inverse
  
  #getInverse Function
  getInverse <- function() invmat
  
  #return List
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invmat <- x$getInverse()
  
  #get cached data
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setInverse(invmat)
  invmat
}
