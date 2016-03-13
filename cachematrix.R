

#The function sets the value of a matrix x, gets the value of x, sets the inverse of x and gets the value of the inverse 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#The function calculates the inverse of the matrix created with the above function. Before doing the calculation it checks if the inverse has already been calculated.
#If it has it gets the inverse from the cache and avoids redoing the calculation. In any other case it calculates the inverse and sets its value in the cache.  


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
