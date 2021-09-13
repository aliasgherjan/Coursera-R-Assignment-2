setwd('C:/Users/DELL/Desktop/rstatistics')


# The R document contains a matrix generating function and a matrix inverse 

## the function that creates the special matrix containing multiple lists

makeCacheMatrix <- function(x = matrix(c(1:2),2,2)) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) a <<- solve
        getsolve <- function() a
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# this function calculates the inverse of matrix

cacheSolve <- function(x, ...) {
        a <- x$getsolve()
        if(!is.null(a)) {
                message("getting inversed matrix")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setsolve(a)
        a
}

makeCacheMatrix(matrix(c(1:4),2))

                