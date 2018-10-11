## R Programming - Week 3 Assignment - GitHub User lcook21

## Function to create a matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
}
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}        

## Function to compute the inverse of makeCacheMatrix matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)   
}
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
        
