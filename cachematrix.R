###test to see if this was synced to GitHub

## this sets up global objects / variables for the Cachesolve function

makeCacheMatrix <- function(x = matrix()) {
        ##this function creates a list of 4 functions
        mtx <- NULL
        set <- function(y) {
                x <<- y
                mtx <<- NULL
        }
        get <- function() x
        setmatrix <- function(cachedinv) mtx <<- cachedinv
        getmatrix <- function() mtx
        list(set = set, get = get,
             setmatrix = setmatrix ,
             getmatrix = getmatrix )
}


## This function will calculate the inverse of a matrix as an input parameter

cacheSolve <- function(x, ...) {
        ##this function will calculate an inverse of the matrix
        ##passed in.  If it's not already stored, it calculates on the fly
        ##if it already stored in cache, it simply pulls from cache
        mtx = x$getmatrix()
        if(!is.null(mtx)) { ##if inverse already stored in cache, pull from cache
                message("This time inverse pulled from cache!")
                return(mtx)
        }
        mat.data <- x$get()
        ##calculate the inverse of the matrix:
        inverse <- solve(mat.data, ...)
        x$setmatrix(inverse)
        return(inverse) ## this is calculating the inverse when not already in cache
}