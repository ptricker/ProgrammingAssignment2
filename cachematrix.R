## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function assigns a list of four functions to x
## First it sets the value of the matrix then it gets it
## Then it sets the value of the inverse of the matrix then it gets it.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL #good
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
##This function returns the inverse of a matrix created with makeCacheMatrix. 
## If the inverse has already been calculated it returns the cached value.
## If it hasn't been calculated on the same matrix it calculates it, then caches it and then 
## returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinv()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(x$get())
        x$setinv(m)
        m
}
