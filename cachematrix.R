## Cached version of matrix inversion. makeCacheMatrix creates an 
## object which contains a matrix and optionally its inverse.
## If the object contains no inverse matrix, cacheSolve calculates
## inverse and store it in the object. If inverse is already stored,
## it will be returned without recalculation.

## Creates an object with matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(mat) {
                    x <<- mat
                    inv <<- NULL
            }
            get <- function() x
            setInv <- function(inverse) inv <<- inverse
            getInv <- function() inv
            list(set = set, get = get,
                 setInv = setInv,
                 getInv = getInv)
}


## Calculate matrix inverse or return its cached value

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
