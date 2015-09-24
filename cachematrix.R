## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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




## Write a short comment describing this function

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


