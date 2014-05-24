
# These functions cache the inverse of a matrix and retrieve the output from cache again. In case of no output in
# cache, the inverse of the matrix is computed.


# This function creates a list of functions (set, get, setinv, getinv) for a matrix 'x'.
# The functions in the list can:
# (1) store a matrix 'x' (set)
# (2) retrieve the matrix 'x' (get)
# (3) cache the inverse of matrix 'x' (setinv), which is computed with the function 'cacheSolve' below
# (4) retrieve the inverse of matrix 'x' (getinv)

makeCacheMatrix<-function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



# The following function computes and prints the inverse of the matrix 'x' that is used as input to the function
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
# the function retrieves the inverse of the matrix from cache.


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}