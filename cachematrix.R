## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The following function `makeCacheMatrix` creates a special or an 
# empty set of values for a "matrix". All of the values for this 
# matrix are represented by a list containing functions to (invertible 
# matrices only work with square matrices with a determinant that isn't 
# equal to zero):

# 1. set the matrix value
# 2. get the matrix value
# 3. set the invertible matrix value
# 4. get the invertible matrix value

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
# The following function checks to see if the calculation for the 
# invertible matrix has already been calculated. If it has already
# been calculated then it gets the invertible matrix from the cache
# and skips calculating it. Otherwise, the invertible matrix is
# calculated and sets the invertible matrix equal to the cache 
# in the `setsolve` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
