####### SARA RODRÍGUEZ
##Assignment: Caching the Inverse of a Matrix


##Creating a "matrix" object that caches its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #inversa
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

##Computing the inverse of the "matrix" returned by makeCacheMatrix. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.
#It is asumed that the matrix is always invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

####Checking
#x <- rbind(c(1,-2), c(-2,1))
#a <- makeCacheMatrix(x)
#a$get()
#cacheSolve(a) #no cache in the first run
