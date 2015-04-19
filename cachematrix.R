## Personally, I think this design is flawed.
# This design forces the caller to manage the cache and know
# when it has been computed.  It encourages sloppy programming
# where cacheSolve() is called defensively.
#
# Furthermore, "setmean" may be called with any value, leading
# to a matrix with an incorrect inverse.
#
# See my alternative version in "cachematrix2.R".

makeCacheMatrix <- function(x = matrix()) {
    # The cache of the inverse of x. 
    xInv <- NULL
    list(
        set = function(m) {
            x <<- m
            xInv <<- NULL
        },
        get = function() {
            x
        },
        setInverse = function(inv) {
            xInv <<- inv
        },
        getInverse = function() {
            xInv
        }
    )
}

# Compute the cache inverse of CacheMatrix cm.
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
