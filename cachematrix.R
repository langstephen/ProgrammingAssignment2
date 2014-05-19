## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Create a list with functions to get/set the matrix and its inverse
# in order to allow the inverse to be cached.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# Calculate the matrix inverse using lists created 
# with the above function. If the inverse has already 
# been calculated then return the cached value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
