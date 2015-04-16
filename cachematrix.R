## Functions that get inverse matrix with ability to cache it

## Create the makeCacheMatrix object which cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ## initiate the inverse property to NULL
        i <- NULL
        
        ##  Set the matrix to caculate
        set <- function(y) {
                x <<- y
                
                ## When set a new matrix, reset its inverse to NULL
                i <<- NULL
        }
        
        ## Get the matrix
        get <- function() x
        
        ## Set the inverse of the matrix to cache
        setinverse <- function(inverse) i <<- inverse
        
        ## Get the inverse of the matrix from cache
        getinverse <- function() i
        
        ## Return the methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x' which is created by makeCacheMatrix.
## It uses cache data first,
## If it's not cached, caculate it and cache it using makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Try to get it from cache
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Get matrix data and calculate its inverse matrix
        data <- x$get()
        i <- solve(data)
        
        ## Cache the inverse matrix
        x$setinverse(i)
        
        ## Return the inverse matrix
        i
}
