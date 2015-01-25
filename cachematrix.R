## Matrix inversion can be an expensive operation.  The following functions
## facilitate the reuse of a previously-computed matrix inverses, through
## caching.

## Create a special "matrix" which is a list containing functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ## The initial value of the inverse is null
    i <- NULL
    ## Function for setting the value of the matrix
    set <- function(y) {
        ## set the (non-inverted) matrix (x) to the input value (y)
        x <<- y
        ## the value of x was just set, so inverse has not yet
        ## been calculated
        i <<- NULL
    }
    ## Function for getting the value of the matrix
    get <- function() x
    ## Function for setting the inverse
    setinv <- function(inv) i <<- inv
    ## Function for getting the inverse
    getinv <- function() i
    ## create list of all functions
    list( set = set, get = get, getinv = getinv, setinv = setinv)
}

## Return the inverse of the matrix.  If the inverse
## has already been computed, use the cached value.
## Otherwise, compute the inverse and cache the value
cacheSolve <- function(x, ...) {
    ## get the inverse
    i <- x$getinv()
    ## check if non-null (indicating that it has not been
    ## previously computed)
    if(!is.null(i)) {
        ## indicate that caced value is being used
        message("getting cached data")
        ## return that value
        return(i)
    }
    ## get the data, the matrix to be inverted
    data <- x$get()
    ## compute the inverse
    i <- solve(data, ...)
    ## store the inverse
    x$setinv(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
