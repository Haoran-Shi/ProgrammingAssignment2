## The two functions attempt to calculate the inverse of an invertible matrix and cache it. 
## If the inverse has already been calculated (and the matrix has not changed), then the the inverse should be retrieved from the cache.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse_m <- NULL
    set <- function(y){
        x <<- y
        inverse_m <<- NULL
    }
    get <- function() x
    set_inverse_m <- function(cal_inverse_m) inverse_m <<- cal_inverse_m
    get_inverse_m <- function() inverse_m
    list(set = set, get = get,
        set_inverse_m = set_inverse_m,
        get_inverse_m = get_inverse_m)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse_m()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    inverse_matrix <- solve(data,...)
    x$set_inverse_m(inverse_matrix)
    inverse_matrix
}
