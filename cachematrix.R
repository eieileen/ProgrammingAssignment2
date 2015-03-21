## These functions can be used to cache the inverse of a matrix to avoid
## redoing time consuming computations. 

## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function that does the following.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        if(!is.matrix(x)) {
                print("Enter a matrix object as the argument.")
                return(NULL)
        } else {
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
}

## cacheSolve first checks if the inverse of your matrix has already been 
## calculated. If so, it returns the inverse and skips the computation.
## Otherwise, it calculates the inverse and sets its value in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
}
