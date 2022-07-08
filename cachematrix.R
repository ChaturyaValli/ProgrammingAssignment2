## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Function: The command "makeCacheMatrix" tends to produce an uncommon "matrix" object. The inverse of the matrix object can be cached by this "matrix" object.


makeCacheMatrix <- function(x = matrix()) {
         e <- NULL
    set <- function(y) {
        x <<- y
        e <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) e <<- inverse
    getinverse <- function() e
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)


}


## Write a short comment describing this function

## Function: The function "cacheSolve" returns the inverse of the unique "matrix" object that the prior "makeCacheMatrix" returns.
##If the matrix is same, the inverse has already been calculated; therefore, cacheSolve must receive the answer from cacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        e <- x$getinverse()
    if(!is.null(e)) {
        message("getting cached data")
        return(e)
    }
    matrix <- x$get()
    e <- solve(matrix, ...)
    x$setinverse(e)
    e

}
