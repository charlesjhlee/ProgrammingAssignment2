## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(inversematrix) im <<- inversematrix
        getinvmatrix <- function() im
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        im <- x$getinvmatrix()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinvmatrix(im)
        im
        ## Return a matrix that is the inverse of 'x'
}
