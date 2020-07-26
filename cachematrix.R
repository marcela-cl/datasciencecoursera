## In this program, we compute the inverse of a square matrix.
## If the inverse has already been calculated, we retrieve the inverse from the cache.
## If not, we compute and store it into cache.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## But, if the inverse has already been calculated, it is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}


# x <- matrix(c(1,2,3,4),2,2)
# x
# y <- solve(x)
# y
# z <- x %*% y
# z
# 
# x1 <- makeCacheMatrix(x)
# 
# y1 <- cacheSolve(x1)
# y1
# y2 <- cacheSolve(x1)
# y2
