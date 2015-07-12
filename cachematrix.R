## Before runing the functions you can create a matrix.
## You can do it using this instructions:
numberRowsColums <- as.integer(runif(1, min = 2, max = 10))
numberCases <- numberRowsColums^2
x <- matrix(runif(numberCases, min = 0, max = 99),numberRowsColums)

## The function cacheMatrix return a list containing a function that:

makeCacheMatrix <- function(x = matrix()) { 
        inverse <- NULL
        set <- function(y) {                              ## 1. Set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                               ## 2. get the value of the matrix
        setinverse <- function(solve) inverse <<- solve   ## 3. set the value of the inverse
        getinverse <- function() inverse                  ## 4. get the value of the inverse
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## The function cacheSolve return a matrix that is the inverse of 'x'.
## x is the name of the list you have creted with makeCacheMatrix.
## To check the function you have to run first makeCacheMatrix()
## or save the list obtained with the function makeCacheMatrix
## in the global environment

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()       ## It first checks to see if the inverse has already been calculated
                                        ## If so, it gets the mean from the cache and skips the computation
        if(!is.null(inverse)) {         
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)     ## Otherwise, it calculates the inverse of the data
        x$setinverse(inverse)           ## and sets the value of the mean in the cache via the setinverse function.
        inverse        
}
