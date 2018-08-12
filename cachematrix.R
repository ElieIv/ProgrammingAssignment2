## The functions in this file are creating an object representing a
## special "matrix" and caches its inverse matrix

## Creates a special "matrix" object that can cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<-NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## Compute the inverse of a special "matrix".
## Return the value from the cache, if already calculated
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
            message("Data from the cache")
            return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
}
