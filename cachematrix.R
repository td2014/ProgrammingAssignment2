## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## This function creates a special type of matrix object
## that can be cached, so that later uses of it may not
## need to redo a calculation that has already been performed,
## like a matrix inversion.
##
## Note:  I utilized the instructions from the course
## as a template for the code implementation.
##

makeCacheMatrix <- function(x = matrix()) {
        m <-  NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_cacheInv <- function(solve) m <<- solve
        get_cacheInv <- function() m
        list(set = set, get = get,
             set_cacheInv = set_cacheInv,
             get_cacheInv = get_cacheInv)
}


## Write a short comment describing this function
##
## This function performs a matrix inversion on a matrix.
## The passed matrix is of the special cacheable type (CacheMatrix).
## If the inverse has already been computed and cached, the
## function below will immediate return the cached result
## otherwise it will compute the inverse, cache it, and then
## return the inverse as the result.
##
## Note:  I utilized the instructions from the course
## as a template for the code implementation.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_cacheInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_cacheInv(m)
        m
}
