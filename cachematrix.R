## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # This will store the cached inverse
        
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset the cached inverse when the matrix is changed
        }
        
        get <- function() {
                x  # Return the matrix
        }
        
        setInverse <- function(inverse) {
                inv <<- inverse  # Cache the inverse
        }
        
        getInverse <- function() {
                inv  # Return the cached inverse
        }
        
        # Return a list of functions to interact with the matrix and its inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if (!is.null(inv)) {  # If the inverse is already cached, return it
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()  # Get the matrix
        inv <- solve(data, ...)  # Calculate the inverse
        x$setInverse(inv)  # Cache the inverse
        inv  # Return the inverse
}
