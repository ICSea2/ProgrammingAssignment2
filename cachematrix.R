## Function to cache the inverse of a matrix instead of doing the rather expensive computation every time

## makeCacheMatrix: create list object to cache matrix inverse(set and get for the matrix and its inverse)
makeCacheMatrix <- function(A = matrix()) {
        m <- NULL
        set <- function(B) {
                A <<- B
                m <<- NULL
        }
        get <- function() A
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve: returns the inverse of the matrix. If first time called, calculate using solve(), else get it from cache
cacheSolve <- function(A, ...) {
        ##get the value from cache, and check if it's null
        m <- A$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if not cached, calculate it
        data <- A$get()
        m <- solve(data, ...)
        A$setsolve(m)
        m
}

