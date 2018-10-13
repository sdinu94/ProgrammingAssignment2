## One of the most common operations while using ML algorithms is to
## find the inverse of a Matrix. Finding an inverse of a matrix is a
## computationally expensive procedure, especially with large datasets.
## It would be useful if we could cache the result of an inverse matrix,
## so that we need not recalculate it, if it is not going to change often.
## The cacheSolve() function helps cache the value of a matrix and retrieve
## the cached value if available. If not, it computes the inverse of the matrix
## and caches it.

## makeCacheMatrix() function which is called inside cacheSolve() function
## creates a special matrix object to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                             ## 'i' is the cached copy of the inverse. It is initialized to NULL
  set <- function(y) {                                  ## if there is a new matrix, the cache is defined using the set() function 
    x <<- y                                             ## store the new matrix in parent environment
    i <<- NULL                                          ## if there is a new matrix, reset inverse to NULL
  }
  get <- function() x                                   ## return the value of the matrix
  setinverse <- function(inverse) i <<- inverse         ## store the value of inverse of matrix in parent environment
  getinverse <- function() i                            ## return the value of inverse of matrix when called
  list(set = set,                                       ## the special list is created so that we can access the functions using
       get = get,                                       ## $ operator
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## cacheSolve() computes the inverse of the matrix which is returned by makeCacheMatrix
## If the matrix has not changed and the inverse has already been created, it returns
## the inverse from the cache. If not, it calculates the inverse and also caches the result
## to the memory

cacheSolve <- function(x, ...) {                       ## The matrix whose inverse needs to be computed is passes to the function
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()                                  ## Tries to get the value of inverse from cache
  if(!is.null(i)){                                     ## If cache is not null, it returns the value from the cache
    message("getting cached data")
    return(i)
  }
  data <- x$get()                                      ## If not cached, uses get() function to get the input
  i <- solve(data, ...)                                ## Uses solve function to compute the inverse
  x$setinverse(i)                                      ## Caches the inverse to the memory
  i
}
