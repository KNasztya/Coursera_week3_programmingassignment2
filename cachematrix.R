# Assignment: Caching the Inverse of a Matrix

# 1.makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# 2. cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



# Examples:
matrix_example1 <- matrix(c(1,2,-1,-1), 2, 2) #define an invertible matrix
matrix_example1 #print the matrix to see
m1 <- makeCacheMatrix(matrix_example1) #create the special "matrix"
cacheSolve(m1) #for the first time calculates the inverse and returns it
cacheSolve(m1) #for this second time message will appaer, that getting cached
#data and inverse will be printed from the cache

# Check if I really get the identity matrix 
# multiplying my matrix with its inverse:
matrix_example1_inverse <- cacheSolve(m1)
matrix_example1%*%matrix_example1_inverse #yes, the result is the identity matrix

