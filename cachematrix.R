## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinversa <- function(inversa) inversa <<- inversa
  getinversa <- function() inversa
  list(set=set, 
       get=get, 
       setinversa=setinversa, 
       getinversa=getinversa)
}


##  Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversa <- x$getinversa()
  if(!is.null(inversa)) {
    message("getting cached data.")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data)
  x$setinversa(inversa)
  inversa
}
