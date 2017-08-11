## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache
## inverse of the matrix. It initially sets a variable "invert"
## stating that the matrix has not been inverted. Once the matrix
## is inverted (using cache Solve) the matrix is cached as the
## inverted value.

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(m) {
                x <<- m
                invert <<- NULL
        }
        get <- function() x
        setInvertMatrix <- function(inverse) invert <<- inverse
        getInvertMatrix <- function() invert
        list(set = set, get = get,
             setInvertMatrix = setInvertMatrix,
             getInvertMatrix = getInvertMatrix)
        
        
}


## This function first tests if the matrix has been inverted
## if it has it returns the cached value. If there is no stored
## value in the cache the function inverts the matrix.

cacheSolve <- function(x, ...) {
        invert <- x$getInvertMatrix()
        if(!is.null(invert)) {
                message("getting cached matrix")
                return(invert)
        }
        mat <- x$get()
        invert <- solve(mat, ...)
        x$setInvertMatrix(invert)
        invert
}
