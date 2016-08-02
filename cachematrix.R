# Creates 'matrix' 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Computes inverse of matrix from makeCacheMatrix function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {                # If same matrix was previously calculated...
        message("getting cached data")
        return(inv)                     # ... just return old result
    }                   
    mat <- x$get()                      # else get uncalculated matrix
    inv <- solve(mat, ...)              # calculate
    x$setInverse(inv)
    inv
}