## The functions makeCacheMatrix() and cacheSolve() work together to efficiently 
## compute and cache the inverse of a matrix, reducing redundant computations 
## and improving performance when the inverse is needed multiple times.


## makeCacheMatrix() function creates a special "matrix" object that stores a given
## matrix and caches its inverse. It provides methods to set or retrieve the matrix
## and its cached inverse, ensuring the inverse is only computed when necessary.

makeCacheMatrix <- function(x = matrix()) {
   if (!is.matrix(x) || nrow(x) != ncol(x) || det(x) == 0) {
      stop("Input must be a square and invertible matrix.")
    }
    
    inv_m <- NULL  
    
    set <- function(y) {
      if (!is.matrix(y) || nrow(y) != ncol(y) || det(y) == 0) {
        stop("Input must be a square and invertible matrix.")
      }
      x <<- y
      inv_m <<- NULL  
    }
    
    get <- function() x
    set_inv_m <- function(inverse) inv_m <<- inverse
    get_inv_m <- function() inv_m  
    
    list(set = set, get = get,
         set_inv_m = set_inv_m,
         get_inv_m = get_inv_m)
  }
  
## cacheSolve() function computes the inverse of the matrix stored within the
## special object created by makeCacheMatrix(). If the inverse has already been 
## calculated, it retrieves the cached result, avoiding redundant computations 
## and enhancing efficiency.

cacheSolve <- function(x, ...) {
    inv_m <- x$get_inv_m()
    
    if (!is.null(inv_m)) {
      message("Getting cached data")
      return(inv_m)
    }
    
    data <- x$get()
    inv_m <- solve(data, ...)  
    x$set_inv_m(inv_m)         
    
    return(inv_m)
    }

## Testing the functions  
m1 = matrix(
  c(30, 10, 10, 25, 10, 14, 20, 30, 40),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
)
m01 <- makeCacheMatrix(m1)

## To use cached matrix, call the below function again. The message will be shown. 
cacheSolve(m01)
