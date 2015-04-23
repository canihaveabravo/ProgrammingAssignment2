## These functions are used cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- Y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## This function computes the inverse of the "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## then the cacheSolve retrieves the inverse from the cache.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
