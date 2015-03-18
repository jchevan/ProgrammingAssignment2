## This function called makeCacheMatrix was developed for R Programming Assign 2
## The code is based on the example caching the mean of a vector
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <-NULL
        set <-function(y) {
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function called cachesolve was developed for R Programming Assign 2
## The code is based on the example caching the mean of a vector
## cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
                
        m <- x$getinv()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinv(m)
                m
        }
}
