## This function creates a matrix and them caches its inverse
## You can enter a matrix X and the function will inverse it and cache the results

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that 
        ## can cache its inverse
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function will return a matrix that is the inverse of 'x'
## You can apply it to variable where the result of the makeCacheMatrix assigned were assigned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
