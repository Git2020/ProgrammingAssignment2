# This pair of functions demonstrates how we can take advantage of lexical
#   scoping rules in R to preserve state inside of an R object. The first
#   function creates a list "wrapper" for a matrix and its inverse, and the
#   second function returns the cached value of the inverse or calculates
#   and returns the inverse if not yet cached.

  # This function creates a list to hold "set" and "get"
  #   functions for a matrix and its cache-able inverse.

  makeCacheMatrix <- function(x = matrix()) {

    # Ensure that the inverse has no value before setting the matrix or inverse:
    i <- NULL

    # Methods to set and get the matrix    
    set <- function(y) {
      x <<- y
      # Wipe out "i" every time the matrix is first set or later reset.
      i <<- NULL
    }
    get <- function() x

    # Methods to set and get the value of the matrix inverse
    setinv <- function(inv) i <<- inv
    getinv <- function() i

    # Return the list of methods for manipulating the matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# This function returns the inverse of a "makeCacheMatrix" object by
#   calculating the matrix inverse if cache is empty, or by returning
#   the cached value of the inverse if cache is not empty.

  cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

    i <- x$getinv()
    
    # Return the matrix inverse's cached value if it is present:
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    
    # Calculate the matrix inverse's value:
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    
    # Return the calculated inverse value:
    i
  }

# Script for testing the correctness of our functional pair.

# Create a "makeCacheMatrix" object, using a non-singular matrix.
  k<-makeCacheMatrix(j <- matrix(1:4, nrow=2, ncol=2))

  # Original computation - should compute and return the matrix inverse.
  cacheSolve(k)

  # Obtain cached version of original computation.
  # We expect the message "getting cached data" to appear this time.
  cacheSolve(k)
  
  # Check: Multiply matrix by assumed inverse to obtain identity matrix
  j%*%cacheSolve(k)
