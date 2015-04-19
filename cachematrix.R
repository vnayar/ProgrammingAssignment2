## Personally, I think this design is flawed.
# This design forces the caller to manage the cache and know
# when it has been computed.  It encourages sloppy programming
# where cacheSolve() is called defensively.
#
# Furthermore, "setmean" may be called with any value, leading
# to a matrix with an incorrect inverse.
#
# See my alternative version in "cachematrix2.R".


# Initialize a "CacheMatrix" object with methods getting
# and setting a matrix, and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # The cache of the inverse of x. 
    xInv <- NULL
    list(
        # Stores a matrix in this CacheMatrix.
        set = function(m) {
            x <<- m
            xInv <<- NULL
        },
        # Retrieves the matrix stored in this CacheMatrix.
        get = function() {
            x
        },
        # Stores a matrix inverse in this CacheMatrix.
        setInverse = function(inv) {
            xInv <<- inv
        },
        # Retrieves the matrix inverse stored in this CacheMatrix.
        getInverse = function() {
            xInv
        }
    )
}

# Compute the cache inverse of CacheMatrix cm.
# The matrix inverse cache of cm will be updated if not set
# and simply returned without recomputation if it is already set.
cacheSolve <- function(cm, ...) {
    inv <- cm$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    x <- cm$get()
    message("computing cache data")
    inv <- solve(x, ...)
    cm$setInverse(inv)
    inv
}
