## Function that creates the methods setMatrix, setInverse, getInverse
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #variable to store the inverse of the matrix
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() x #Returns the matrix
    setInverse <- function(inverse) inv <<- inverse  #Function that sets the inverse of a matrix
    getInverse <- function() inv  #Function that returns the inverse of a matrix
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function that evaluates in first place is the invert matrix has been previously calculated
## if so it returns it value, otherwise the invert matrix is calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) { #Check is the inv variable has value if so returns it
        message("Already have values cached")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}
