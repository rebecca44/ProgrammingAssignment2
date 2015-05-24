## Below are two functions that are used to create a special object that stores a matrix 
## and caches its inverse.

## The makeCacheMatrix function creates a set of functions to store a matrix (set), 
## return the stored matrix (get), cache the inverse of the matrix (setinverse)
## and return the cached inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        ## Store a new matrix to cache and reset the cached inverse to NULL
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## The cacheSolve function uses the functions created by makeCacheMatrix to return the inverse
## of the stored matrix from cache if it already exists, otherwise it calculates the inverse and 
## caches it via the setinverse function

cacheSolve <- function(x, ...) {
    ## Check if the inverse matrix is stored in cache
    m <- x$getinverse()
    if(!is.null(m)) {
        ## Inverse matrix is stored in cache so return it from cache
        message("Getting cached data")
        return(m)
    }
    ## Inverse matrix is not stored in cache.
    ## Retrieve matrix
    data <- x$get()
    ## Calculate inverse
    m <- solve(data,...)
    ## Store inverse to cache
    x$setinverse(m)
    ## Return inverse
    m
}
