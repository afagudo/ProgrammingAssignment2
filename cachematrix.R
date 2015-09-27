## This function creates a special "matrix" object that can cache its inverse.
# Creates a special "matrix" object that can cache its inverse.
# Saves the matrix to variable x and its inverse to variable inv in scope.
# The returned list contains: set, get, setInv and getInv
# set: sets matrix and resets cached inverse
# get: returns matrix
# setInv: saves inverse value
# getInv: returns cached inv value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() {
        x
        }
        setInv <- function(inverse){ 
        inv <<- inverse
        }
        getInv <- function() {
        inv
        }
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
# Takes the object of that type as an argument 'x', checks if the inverse value is already
# cached, and if it is returns the cached value; if not, this function calculates the
# inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setSolve'
# and returns the result.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
