## The following functions reduce the computational cost of calculating 
## a matrix's inverse by saving it to the cache after first calculation
## and retrieving it when needed instead of recalculating every time.

## function that creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        ## function to set the matrix in the cache
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## function to call the matrix from the cache
        get <- function() x
        ## function to solve for the inverse if not in the cache
        setsolve <- function(solve) inv <<- solve
        ## function to call the inverse from the cache
        getsolve <- function() inv
        ## create list of the functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## function that checks the cache for the inverse if it's not cached, 
## the function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above.
cachesolve <- function(x, ...) { 
        ## check if the inverse is in cached:
        ## call on the inverse
        inv <- x$getsolve()
        
        if(!is.null(inv)) {
                ## if inverse is cached, call and return it
                message("getting cached inverse")
                return(inv)
        }
        ## if inverse is not cached, compute the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
		## return matrix that is the inverse of x
        inv
}