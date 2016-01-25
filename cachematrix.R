# This is the code for executing the second R coding assignment
# The first of the two functions creates a matrix object using the assigned funtion name.
# The second of the two functions computes the inverse of the function created in the first function
# makeCacheMatrix creates the matrix
# cacheSolve calculates its inverse


makeCacheMatrix <- function(x = matrix()) {
    inverse.mat <- NULL
    set <- function(y) {
        x <<- y
        inverse.mat <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) inverse.mat <<- inverse
    get.inverse <- function() inverse.mat
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse.mat <- x$get.inverse()
    if(!is.null(inverse.mat)) {
        message("getting cached data")
        return(inverse.mat)
    }
    mat.solve <- x$get()
    inverse.mat <- solve(mat.solve, ...)
    x$set.inverse(inverse.mat)
    inverse.mat
}
