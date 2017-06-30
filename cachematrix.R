## Matrix inversion can be a time-consuming computation.
## Because of this, benefit can be found in caching the inverse
## of a matrix instead of repeatedly computing it.
## This pair of functions is made to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##holds cached value(initially set to NULL 
        ##b/c nothing is cached)
        inver <- NULL
        
        ##stores matrix
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        
        ##returns stored matrix
        get <- function()x
        
        ##cache inverse
        set_inverse <- function(inverse) inver <<- inverse
        
        ##gets cached value
        get_inverse <- function() inver
        
        ##returns list where each named element is a function
        list(set = set, get = get, 
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## This function computes the inverse of the "matrix" returned in the
## makeCacheMatrix function above. If the inverse of the matrix 
## has been calculated (and the matrix unchanged), then this function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ##gets cached value
        inver <- x$get_inverse()
        
        ##if cached value exists, return it
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        
        ##otherwise, get the matrix, calc inverse, and store it
        matrix <- x$get()
        inver <- solve(matrix, ...)
        x$set_inverse(inver)
        
        ##returns inverse
        inver
}
