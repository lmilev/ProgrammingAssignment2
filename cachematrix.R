## Cachable matrix sample usage:
# m <- makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,1), 3, 3))
# > cacheSolve(m)
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
## Luckily the inverse of an identity matrix... is the identity matrix
#> cacheSolve(m)
# getting cached data
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
#> m <- makeCacheMatrix(matrix(1:4, 2, 2))
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## makeCacheMatrix returns a list with names that give access to 
## functions to access or cache solution of a matrix. 
## i.e. mc <- makeCacheMatrix(matrix(1:4,2,2))
## mc$get() would print the 2 by 2 matrix
## the main purpose of the returned object is to be used by
## the other function called cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  # can hold the result of solve(x)
  solution <- NULL
  
  # sets the underlying raw matrix, and resets cached solution
  set <- function(y) {
    x <<- y
    solution <<- NULL
  }
  # gets the underlying raw matrix
  get <- function() x
  
  # sets cached solution of x
  setsolution <- function(solved) solution <<- solved
  # gets cached solution of x, if any
  getsolution <- function() solution
  
  # returns an object from which we can
  list(set = set, # change the underlying matrix
       get = get, # get the underlying matrix
       setsolution = setsolution, # cache the solution of the underlying matrix
       getsolution = getsolution) # retrieved cached solution of the underlying matrix
}


## cacheSolve
## x- acceptable input is the result of makeCacheMatrix
## ... - these will be forwarded directly to solve,
## however they dont participate in the decision process if solution should read from cache or not
##
## Solves the underlying matrix stored in x, 
## if solution is not already stored in x.
## Returns solution either way.
##
## For illustrative purposes also prints "getting cached data" when
## it does not solve, but only reuses the cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolution()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  # underlying matrix
  data <- x$get()
  s <- solve(data, ...)
  x$setsolution(s)
  s
}
