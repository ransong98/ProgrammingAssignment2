## The following functions that store the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # store inverse value
        inv <- NULL
        # set the original matrix and reset inverse
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        # get the original matrix
        get <- function()x
        # set inverse value
        setInverse <- function(solveMatrix) inv <<-solveMatrix
        # get inverse value
        getInverse <- function() inv
        # Returns a list of the 4 functions, this list is the special "matrix"
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
