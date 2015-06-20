## Put comments here that give an overall description of what your
## functions do

## The function createMatrix creates a new square matrix 'x
 ## First we create a matrix x. We can do using this instructions:
numberRowsColums <- as.integer(runif(1, min = 2, max = 10))
numberCases <- numberRowsColums^2
x <- matrix(runif(numberCases, min = 0, max = 99),numberRowsColums)


}

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## The function cacheSolve return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse        

}
