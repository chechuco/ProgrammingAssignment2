## This functions set an ad-hoc matrix object, computes its inverse
## and stores it in a cache.
## If the inverse has already been calculated, get the result from the cache.

## makeCacheMatrix create a list of functions:
##  ·set         set a matrix object
##  ·get         get the matrix object
##  ·setinverse  set the inverse of the matrix object 
##  ·getinverse  get the inverse of the matrix object

makeCacheMatrix <- function(x = matrix()) {
    ## Create the functions
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    ## Returns a list with the functions 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix set by makeCacheMatrix
## If it has been already computed, get the result from the cache

cacheSolve <- function(x, ...) {
    ## Look for the inverse in the cache
    m <- x$getinverse()
    if(!is.null(m)) {
        ## The inverse is in the cache: return it
        message("getting cached data")
        return(m)
    }
    ## The inverse is not in the cache: calculate, store and return it
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
