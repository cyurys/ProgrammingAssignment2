## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## There are two functions here:
## makeCacheMatrix -- to create and
## cacheSolve -- to compute the inverse (or return already computed value)


## This function creates a special "matrix" object that can cache its inverse.
## Use casheSolve function instead of solve function if inverse is used repeatedly.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieves it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv	
}
