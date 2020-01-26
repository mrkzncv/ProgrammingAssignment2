## My functions cache the value of the inverse matrix of a given matrix X
## this will be useful if sometime later we again need to obtain
## the inverse matrix of X and not recompute it

## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to set and get value of the matrix X
## and the inverse matrix X

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(store) i <<- store
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##The following function first checks if the inverse of X has already been calculated.
## If so, it gets the inverse X from the cache and skips
##Otherwise, it calculates the inverse X of the data and sets
##the value of the inverse X in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

# Check if it works or not
x <- matrix(1:4,2,2)
x
new_x <- makeCacheMatrix(x)
cacheSolve(new_x)