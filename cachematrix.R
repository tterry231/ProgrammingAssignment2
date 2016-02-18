## This function creates a special matrix object that can cache its inverse

## x is assumed to be a square invertible matrix

## The return is a list containing functions to resolve the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {

        inv = NULL

        set = function(y) {
                x <<- y
                inv <<- NULL
        }

        get = function() x

        setinv = function(inverse) inv <<- inverse 

        getinv = function() inv

        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix hasn't changed then the function
## retrieves the inverse from cache

## x is the output of makeCacheMatrix
## Return is the inverse of the original matrix either calculated or from cache
## First check is to determine if inv is NULL, not in cache, or already populated
## If inv is NULL then get the inverse using the solve() function and set inv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv = x$getinv()
        
        if (!is.null(inv)){

		return(inv)

        }
        
        mat.data = x$get()

        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)


}
