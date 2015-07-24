## The following two functions implements a cacheable "matrix" object (see test cases). 

## Initiate and returns a cacheable "matrix" object.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL # If x is set/reset the inverse is not yet computed. 
  }
  get <- function() x
  setinvers <- function(invers) i <<- invers
  getinvers <- function() i
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}

## Returns the invers of x.  
cacheSolve <- function(x, ...) {
  i <- x$getinvers()
  # If i is not null return the inverse  
  if(!is.null(i)) {
    return(i)
  }
  # ... else compute the inverse. 
  data <- x$get()
  i <- solve(data, ...)
  x$setinvers(i)
  i
}

# Test case 1
m_1 <- makeCacheMatrix(matrix(c(2,3,2,3,4,5,5,6,7), 3, 3))
print(m_1$get() == matrix(c(2,3,2,3,4,5,5,6,7), 3, 3))
print(is.null(m_1$getinvers()))
cacheSolve(m_1)
print(round(m_1$getinvers() %*% m_1$get()) == diag(3))

# Test case 2
temp_m <- matrix(sample.int(100, size = 25, replace = TRUE), nrow = 5, ncol = 5)
m_2 <- makeCacheMatrix(temp_m)
print(m_2$get() == temp_m)
print(is.null(m_2$getinvers()))
cacheSolve(m_2)
print(round(m_2$getinvers() %*% m_2$get()) == diag(5))