## Memoized version of matrix inversion

## Creates a inverse-cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(newInverseMatrix) {
            inverseMatrix <<- newInverseMatrix
        }
        getInverseMatrix <- function() inverseMatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)

}


## Gets the inverse the matrix stored in x
## The first time it is called, stores the
## result. From than moment on, further
## invocations store the cached result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    candidateInverse <- x$getInverseMatrix()
    if(!is.null(candidateInverse)){
        message("getting cached inverse")
        candidateInverse
    } else {
        matrixToInvert <- x$get()
        candidateInverse <- solve(matrixToInvert)
        x$setInverseMatrix(candidateInverse)
        candidateInverse
    }
}
