## These two functions create a special matrix that can store it's content and inverse
## as well.  Computing the inverse is only done if it has not previously been computed.

## Creates the special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get,
        setinverse=setinverse, 
        getinverse=getinverse)
}


## Computes the inverse of the "special" matrix unless it has a cached value to return.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    message("calc. and set new inverse")
    matrx <- x$get()
    inv <- solve(matrx,...)
    x$setinverse(inv)
    inv   
}
