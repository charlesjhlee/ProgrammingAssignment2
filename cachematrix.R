
#R_Programming
#Assignment 2

## The functions listed here are used to calculate an inverse of a matrix
## and store it in a cache (i.e. in a separate environment).
## Thereafter, upon calling a function to calculate inverse of a matrix,
## it is checked whether there is inverse matrix already calculated and
## saved in cache. If the inverse matrix exists in the cache, then this
## inverse matrix is called, otherwise inverse of matrix is calculated.




## The below function is used to store 4 functions. Set stores the original matrix
## into cache, get calles on the original matrix, setinvmatrix stores
## inverse of the matrix in the cache, and getinvmatrix calls on the
## inverse of the matrix from cache if it exists.

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



## The below function is used to first check whether the inverse matrix exists
## in the cache. If it exists, then the function simply calls on this inverse
## matrix from cache and the function stops. If the inverse matrix does not exist
## then the inverse of the matrix is calculated and the results are returned.

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
}
