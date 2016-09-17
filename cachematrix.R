## We define functions that can cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse, it is a list containing functions to
## set the value of a matrix, get the value of a matrix, set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get, set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


## This function calculates thie inverse of the special matrix created above. If the inverse is already calculated,
## it gets it from the cache and skips the computation. Otherwise, it computes the mean and set the value of the
## inverse via set_inverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set_inverse(inv)
    inv
}
