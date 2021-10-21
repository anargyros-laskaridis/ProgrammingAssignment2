## As calculating matrix inversion is expensive the following functions only
## do it once and cache the result for future use.

## This creates a "special" matrix which is in fact a list of functions that
## set the value of the matrix
## get the value of the matrix
## set the value of the inversed matrix
## get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
	inversedMatrix <- NULL
	set <- function(y) {
		x <<- y
		inversedMatrix <<- NULL
	}
	get <- function() x
	setMatrixInverse <- function(inversedMatrixResult) inversedMatrix <<- inversedMatrixResult
	getMatrixInverse <- function() inversedMatrix 
	list(set = set, get = get,
		setMatrixInverse = setMatrixInverse ,
		getMatrixInverse = getMatrixInverse)
}


## Search and return the cached version of the inverted matrix or calculate and store
## the inverse matrix in the cache and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversedMatrix  <- x$getMatrixInverse()
        if(!is.null(inversedMatrix )) {
                message("getting cached inversed matrix")
                return(inversedMatrix)
        }
        data <- x$get()
        inversedMatrix  <- solve(data)
        x$setMatrixInverse(inversedMatrix)
        inversedMatrix  
}
