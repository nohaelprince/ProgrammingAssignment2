## The two functions below, cache the inverse of a matrix x.
#  This will lower the computational cost when inverse is used frequently.

#  This function creates a special "matrix" object 'x' that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) x_inv <<- inv
    getinv <- function() x_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)   
}


## This function checks if the inverse has already been calculated, 
#  then the function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
   
    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    # Otherwise, calculate the inverse using solve()
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setinv(x_inv)
    x_inv
}
