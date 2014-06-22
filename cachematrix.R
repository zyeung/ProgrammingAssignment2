## Coursera: R Programming Assignment II

## Introduction :
## The two functions embed in this scripts are
## aimed to save a cached version of the caculation
## results and use it in upcoming caculation

## The folowing functions will create cache
## of the input matrix with its corresponding
## inversed value
makeCacheMatrix <- function(x = matrix()) {
	# initialize the inversed value
	inv <- NULL

    ## set the matrix
    set <- function( matrix ) {
            x <<- matrix
			## The matrix is changed
			## reseting the inversed value
            inv <<- NULL
    }

    ## get the matrix
    get <- function() x

    ## set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    ## get the inverse of the matrix
    getInverse <- function() inv

    ## return the list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function will caculate the inverse of a matrix.
## It firstly check via first function. If the inverse value
## has already been in cache, it will get the value from the 
## cache. If not, the function will calculates the inverse of the 
## matrix and sets the value of in the cache via the method 
## in first function.
cacheSolve <- function(x, ...) {
    ## check if the inverse value is in cache
	## if so, it will return the inverse of 'x'
    i <- x$getInverse()
    if( !is.null(i) ) {
		message("getting cached data")
		return(i)
    }
	
	## if not cached, get the matrix into data
    data <- x$get()

    ## compute the inverse using %*% operator in R
    inv <- solve(data) %*% data

    ## set the inverse to the object in cache
    x$setInverse(inv)
    ## return the inverse matrix
    inv
}
