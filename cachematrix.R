## The overall goal of the following two functions is to cache the calculation of the inverse of 
## a matrix to avoid computing it repeatedly 

## This function creates a special matrix object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y){
                x <- y
                inv <- NULL
        }
        get <- function() x
        setInverse <- function (inverse) inv <<- inverse
        getInverse <- function () inv
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special matrix object returned from makeCacheMatrix 
## function above.The function returns the inverse stored in the special matrix object if it exists;
## otherwise it computes it using the solve function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
                message ("getting cached inverse")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
