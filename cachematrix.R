## This funtion returns a list where there is entries to read a matrix
## and its inverse as well as functions to define their values.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns a stored inverse or calculates it if it does
## not exist or if the stored matrix and its inverse do not correspond
## to each other.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        matrix <- x$get()
    	is.identity <- function(I) {
            result <- TRUE
            for(k in 1:nrow(I)) result <- (result & (abs(I[[k,k]]-1) < 1e-8))
            if((sum(I)-nrow(I)) < 1e-8) return(result)
            return(FALSE)
        }
        if(!is.null(inverse)) {
            if(is.identity(matrix %*% inverse)) {
                message("getting cached data")
                return(inverse)
            }
            message("stored inverse do not correspond to stored matrix, ")
        }
    	message("calculating...")
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}

