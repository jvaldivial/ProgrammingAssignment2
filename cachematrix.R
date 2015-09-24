## Cached Matrix Inversion - R code
## written by Jose Valdivia L.  - 23th September 2015
## (based on R code from "R Programming" course at Coursera

## The two functions below implements a cache for a matrix inversion
## in order to optimize the performance of this time consuming computation.
## Example of use:
##     m <- matrix(1:4, nrow=2, ncol=2)
##     mm <- makeCacheMatrix(m)
##     cacheSolve(mm)             # First time the inverted matrix is computed
##     cacheSolve(mm)             # Following times the inverted matrix is read 
##                                # from cache. No futher computations.


## The function "makeCacheMatrix" below receives as input the matrix to 
## be inverted, and return a list of 4 functions:
## get() to return the value of the matrix
## set() to set the value of the matrix
## getinv_mat() to get the inverted matrix
## setinv_mat() to set the value of the inverted matrix
## You need to call this function before call the "cacheSolve" function.

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinv_mat <- function(im) inv_mat <<- im
        getinv_mat <- function() inv_mat
        list(set = set, get = get,
             setinv_mat = setinv_mat,
             getinv_mat = getinv_mat)
}




## The fucntion "cacheSolve" below receives as input the list of functions
## created by the function "makeCacheMatrix" for a particular matrix to be 
## inverted. Before it makes the actual matrix inversion, it gets the value 
## from the cache; if the value is not available, then it performs the computation
## and stores the output in the cache using the setinv_mat() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv_mat <- x$getinv_mat()
        if(!is.null(inv_mat)) {
                message("getting cached data for the matrix")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$setinv_mat(inv_mat)
        inv_mat
}


