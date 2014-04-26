## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates matrix object to set and get the inverse of a mtrix

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
    set <- function(y) {
                x <<- y
                m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverse) m <<- inverse
    getInverseMatrix <- function() m
    list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)

}


## Write a short comment describing this function
## This function use makeCacheMatrix to compute the inverse of a matrix
## using memoization

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}
