## Functions for inversions of a matrix.
## Since inversing a matrix can be costly, these functions will
## inverse the matrix and then cache the data.

## to use: 
##
## a <- c=rbind(c(1, -1/4), c(-1/4, 1))  # create matrix
## b <- makeCacheSolve(a)  # create an object for that matrix
## cacheSolve(b) # send that object to cacheSolve
##
## credit where credit is due, this was figured out by reading
## https://class.coursera.org/rprog-034/forum/thread?thread_id=56

## This function creates a special "matrix" object that can cache its inverse.

makeCacheSolve <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solution) m <<- solution
    getInverse <- function() m
    list (set = set, get = get, setInverse = setInverse, 
          getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
