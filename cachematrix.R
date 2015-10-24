## This file contains functions used for caching a matrix inverse:
## makeCacheMatrix - creates a special matrix that can cache its inverse
## cacheSolve - works on the special matrix taking advantage of its cache

##
# Given an R matrix `x` create a cacheMatrix.
# Args:
#       x - the original R matrix
# Returns:
#       list (cacheMatrix) of the following utility functions:
#               $get - return the original x matrix
#               $set - set the x matrix to a new value
#
#               (NOTE: the following two methods should not be used explicitly;
#               rather than using them directly, use the `cacheSolve` function)
#
#               $getcachedinv - get pre-computed inverse of x
#                       (note: it does *not* compute the inverse itself)
#               $setcachedinv - set (cache) the inverse of x
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # get/set original R matrix
        get <- function () x
        set <- function (y) {
                x <<- y         # change the 'x' matrix in the *upper* environ
                inv <<- NULL    # changing the matrix invalidates the inverse
        }
        # get/set cached inverse
        getcachedinv <- function () inv
        setcachedinv <- function (inverse) inv <<- inverse

        # create (and return) the cache matrix as a list
        list(get = get,
             set = set,
             getcachedinv = getcachedinv,
             setcachedinv = setcachedinv,

             # NOTE: IMHO rather than `cacheSolve` it would be easier to use:
             getinv = function () {
                if (is.null(inv)) inv <<- solve(x)
                inv
             })
}


##
# Get the invserse of a given 'cacheMatrix' x (as returned by makeCacheMatrix)
# Args:
#       x - a 'cacheMatrix' list
# Returns:
#       a R matrix that is an inverse of x$get() (the matrix held in x)
# PRE:
#       the x cacheMatrix holds inversible matrix
# POST:
#       caches the result (unless it's cached already)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getcachedinv()
        if (!is.null(inv)) {    # if inverse is cached, simply return it
                message("getting cached data")
                return(inv)
        }
        # here: inverse is *not* cached:
        # compute (solve) it and cache for future use
        inv <- solve(x$get(), ...)
        x$setcachedinv(inv)
        inv
}