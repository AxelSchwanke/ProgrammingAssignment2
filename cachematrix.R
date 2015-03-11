## This file contains two functions for caching the inverse of a matrix.
## This is especially useful, if the matrix inverse is repeatedly use, e.g.
## within a loop


## makeCacheMatrix creates a special object that can cache
## the inverse of a matrix
## argument: x - matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse (NULL = not computed)
        inv <- NULL
        ## create function to set matrix
        set <- function(y) {
                ## set matrix
                x <<- y
                ## initialize inv
                inv <<- NULL
        }
        ## create funtions to get matrix, set and get inverse
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        ## return the list of functions
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the matrix returned by 
## makeCacheMatrix.
## argument: x - matrix-object, returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## get the inverse of the matrix
        inv <- x$getinv()
        ## check, if inv is NOT NULL (already computed before)
        if(!is.null(inv)) {
                message("getting cached data")
                ## return the inverse
                return(inv)
        }
        ## otherwise get matrix
        data <- x$get()
        ## and compute the inverse
        inv <- solve(data, ...)
        ## cache the inverse
        x$setinv(inv)
        ## and return it
        inv
}

## tested with
## x <-  matrix(c(4,3, 3,2), nrow = 2, ncol = 2, byrow = FALSE)
## x$get()
##       [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## matrix<-makeCacheMatrix(x)

## first call -> cache
## cacheSolve(matrix)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## second call
## getting cached data
## cacheSolve(matrix)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
