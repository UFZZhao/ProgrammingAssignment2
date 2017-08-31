## Caching the inverse of a Matrix

## makeCacheMatrix function is to create a special matrix that can catch its inverse.

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) v <<- solve
        get_inverse <- function() v
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## cacheSolve is to compute the inverse of the matrix returned by makeCacheMatrix above. The cachesolve will retrieve the inverse from the cache if the inverse has been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$get_inverse()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$set_inverse(v)
        v
}

## For test
M=matrix(1:4,2,2)
M1=makeCacheMatrix(M)
cacheSolve(M1)
