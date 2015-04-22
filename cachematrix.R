## Submission from Alex Kwan
## R Programming
## Programming Assignment 2



## This function, makeCacheMatrix, creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mm <- NULL
  set <- function(y) {
    x <<- y
    mm <<- NULL
  }
  get <- function() x
  setinvmat <- function(solve) mm <<- solve
  getinvmat <- function() mm
    
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it `get`s the inverse
## matrix from the cache and skips the computation. Otherwise, it 
## calculates the inverse matrix of the initial matrix and sets the 
## value of the initial matrix in the cache via the `setinvmat` function.
cacheSolve <- function(x, ...) {
  mm <- x$getinvmat()
  if (!is.null(mm)) {
    message("getting cached inverse matrix")
    return(mm)
  }
  init_mat <- x$get()
  mm <- solve(init_mat)
  x <- setinvmat(mm)
  mm
}
