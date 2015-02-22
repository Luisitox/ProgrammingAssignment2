## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list of functions that are used to store the values of the different matrices.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                ## initialize cache variable
        set <- function(y) {     ## stores the matrix and the m value indicates that nothing has been computed yet
                x <<- y
                m <<- NULL
        }                       
        get <- function() x      ## takes the initial matrix value
        setinverse <- function(inverse) m <<- inverse  ## changes the value of m for the inverse matrix
        getinverse <- function() m ## takes the inverse matrix value
        list(set = set, get = get, ## makes a list of all the functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function checks if the inverse matrix has already been calculated and if not it does the calculation

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
