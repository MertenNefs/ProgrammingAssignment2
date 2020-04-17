## The two functions below make it possible to invert a matrix, save it to cache and the next time the code
## is run, producing the cached result instead of computing the Solve function again.

## The function makeCacheMatrix takes a matrix as argument, inverts it using the Solve function,
## and saves it as 'm' in the global environment cache.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinvert <- function(solve) m <<- solve
      getinvert <- function() m
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
}

## Function CacheSolve returns the inverted matrix stored in cache, IF there is one (m!=null).
## Otherwise it runs the Solve function and stores inverted matrix 'm' in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinvert()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinvert(m)
      m
}

##test matrix: if you run the 4x4 matrix 'mat' in makeCacheMatrix it gives 'mat1'
mat<- matrix(sample(16),4,4)
mat1<-makeCacheMatrix(mat)
##running cacheSolve(mat1) once, produces the inverted matrix
##repeating cacheSolve(mat1) gives "getting cached data" followed by the inverted matrix