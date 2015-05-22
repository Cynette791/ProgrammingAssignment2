## The function makeCacheMatrix creates a list "vector" that consist of the following functions:
##	1. set : set value of the vector
##	2. get : set value of the vector
##	3. setinverse : set value of the inverse
##	4. getinverse : set value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is set to calculate the inverse of 
## a given matrix using functions that are 
## listed by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()			## Loading the cached inverse of a matrix (if any) to variable 'm'.
        if(!is.null(m)) {			## Checking if there's any inverse cached.
                message("getting cached data")	## If there IS a cached matrix, the function stops here...
                return(m)			## and returs the cached matrix.
        }
        data <- x$get()				## If there's NO inverse cached, the function follows by loading the matrix
        m <- solve(data, ...)			## Proceed to inverse the matrix
        x$setinverse(m)				## Caching the inverse value 
        m					## Printing the inverse (m)
}