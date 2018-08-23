## Put comments here that give an overall description of what your
## functions do

## I am using the "makeCasheMatrix" function to create what is essentially a list
## of four functions. 

## The following fuunction essentially creates a list of four functions to
## set the value of the matrix, get the matrix, set the inverse of the matrix 
## and get it.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
                x <<- y
                inv <<- NULL
        }			## This is to pass the matrix to inverse to the input
                    ## of the makeCacheMatrix function.
        get <- function() x	## This is to return the matrix x, which is 
                              ## the matrix to inverse
        setinverse <- function(inverse) inv <<- inverse
        ## pass the value of the invsersed matrix to m	
	getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
 ## create the list of four functions to use
}


## The following function calculates the inverse of the special matrix
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates 
##the inverse of the data and sets the value of the inverse in the cache via 
## the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinverse()
        ## This is to call the getinverse function from the list generated before
	if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
	 ## If the given matrix is not in the cache, calculate its inverse 
        ## and put it into the cache.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
