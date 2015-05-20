## The file contains two methods.  makeCacheMatrix() returns a list of named
## functions to store and retrieve a cached inverse matrix.  cacheSolve() 
## computes the inverse matrix for an invertable matrix.  cacheSolve() uses
## the list returned from makeCacheMatrix() to retrieve or store the cached
## value.  


## Returns a special "list".  The list contains named functions to retrieve
## input matrix, set the input matrix, retrieve the cached inverse matrix, and
## set the new cached matrix value.
##
## Parameters
##     x - An invertible matrix.  Default is an empty matrix.
makeCacheMatrix <- function(x = matrix()) {
    cached_inverse_matrix <- NULL
    
    # used to set the matrix in the outer environment
    # and resets the cached matrix
    set <- function(new_matrix) {
        x <<- new_matrix
        cached_inverse_matrix <<- NULL
    }
    
    # retrieves the input matrix
    get <- function() {
        x
    }
    
    # sets the cached inverse matrix
    set_cached_inverse_matrix <- function(cached_matrix) {
        cached_inverse_matrix <<- cached_matrix
    }
    
    # retrieves the cached matrix
    get_cached_inverse_matrix <- function() {
        cached_inverse_matrix
    }
    
    # returns a list object containing the functions
    # each element is a named function
    list(set = set,
         get = get,
         set_cached_inverse_matrix = set_cached_inverse_matrix,
         get_cached_inverse_matrix = get_cached_inverse_matrix)
}


## Computes, caches, and returns the inverse matrix for "x".  The method 
## assumes that the input matrix is invertible.
##
## Parameters
##     x - The special "list".  It contains methods to store and retrieve
##         the cached inverse matrix.
cacheSolve <- function(x, ...) {
    cached_inverse <- x$get_cached_inverse_matrix()
    
    # check if the cache value exist before computing the inverse matrix
    if(!is.null(cached_inverse)) {
        message("Cached inverse matrix value found; it will be returned.")
        return(cached_inverse)
    }
    
    # otherwise retrieve the matrix passed into the special "list" and compute
    # its inverse
    input_matrix <- x$get()
    inverse_matrix <- solve(input_matrix)
    x$set_cached_inverse_matrix(inverse_matrix)
    
    # return the inverse matrix
    inverse_matrix
}
