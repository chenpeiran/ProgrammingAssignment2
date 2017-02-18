## Cache the Inverse of a Matrix
## The purpose of these functions is to calcuate the inverse of a matrix first time only.
## The result is cached for future usage to save computing time and resource.


## The makeCacheMatrix function creates an object having the original matrix and 
## callable functions to calculate inverse and save the result in the object itself
## x variable saves the original matrix
## s variable saves the calcuated result of inverse of matrix, it is empty when initiated.
## set function provides a method to save the matrix, or x variable
## get function provides a method to return the matrix, or x variable
## setsolve function provides a method to save the cached results, or s variable
## getsolve function provides a method to return the cached results, or s variable
## The function returns a List of the above 4 functions

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve function uses the Values (a List) from makeCacheMatrix and try to get the 
## cached inverse result. If there is no cached result, it calls solve() fuction to get it
## and save it by calling setsolve() method.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    r <- x$getsolve()
    if(!is.null(r)) {
        message("getting cached data")
        return(r)
    }
    data <- x$get()
    r <- solve(data, ...)
    x$setsolve(r)
    r
}
