## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.

## The below function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { # start makeCacheMatrix
  # stores the cached value
  # initialize to NULL
  inv <- NULL
  set <- function(y) { # to create matrix in working environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse # invert the matrix and store in cache
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # return to the working environment
}                                          # end makeCacheMatrix

## The below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {  # start cacheSolve
  inv <- x$getinverse() # seek the inverse of the matrix from cache
  if(!is.null(inv)) { # gives matrix inverse from cache if it exists, otherwise create the matrix in working environment 
    message("getting cached data.")
    return(inv) # to display matrix
  }                   
  matrix <- x$get()
  inv <- solve(matrix, ...)                # solve(matrix) returns the inverse
  x$setinverse(inv)
  inv
}                                          # end cacheSolve
