## Creates a 'CacheMatrix' object.
makeCacheMatrix <- function(x = matrix()) {
    # The cache of the inverse of x. 
    xInv <- NULL
    list(
        # Sets matrix and clears its inverse cache.
        # @param {matrix} m  The desired matrix to store.
        set = function(m) {
            x <<- m
            xInv <<- NULL
        },
        # Returns previously set matrix.
        # @return {matrix}  The stored matrix.
        get = function() {
            x
        },
        # Returns the matrix inverse, recomputing if necessary.
        # @param {...}  Extra arguments to be passed to "solve".
        # @return {matrix}  The inverse of the stored matrix.
        getInverse = function(...) {
            if (!is.null(xInv)) {
                message("getting cached data")
                return(xInv)
            }
            message("computing cache data")
            xInv <<- solve(x, ...)
            xInv
        }
    )
}
