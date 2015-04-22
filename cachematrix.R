## Coursera R Programming Assignment #2
## Functions to create and calculate the inverse of a matrix, 
## caching the result for future retrieval.

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## set stores the input matrix and initializes i to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## get returns the input matrix
        get <- function() x
        
        ## setinverse stores the inverse matrix
        setinverse <- function(solve) i <<- solve
        
        ## getinverse returns the previously stored inverse matrix
        getinverse <- function() i
        
        ## creates a list of the four functions: set, get, setinverse, getinverse
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse
             )
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix.  If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        ## if the inverse has already been calculated, return it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## else get the matrix,
        data <- x$get()
        ## calculate the inverse,
        i <- solve(data, ...)
        ## store the result, 
        x$setinverse(i)
        ## and return the result
        i
}
