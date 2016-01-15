## This pair of function can solve time for effecient computation of matrix 
##inverse


## This function creates a matrix with four variables (setting data, 
## getting data,setting inverse, getting inverse)

makeCacheMatrix <- function(x = matrix()) {  
	  inv <- NULL   		## setting an empty vector
        set <- function(y) {	## function to set the input matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x	## function to return the input matrix stored
        setinv <- function(inv) inv <<- inv ## function to set the inv of input matrix
        getinv <- function() inv			## function to return the the inverse of the input matrix stored
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function caches the inverse matrix if already computed, if not it calculate it

cacheSolve <- function(x, ...) {inv <- x$getinv()
        if(!is.null(inv)) {		## get cached data if already computed
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)	##compute the inverse of the matrix
        x$setinv(inv)
        inv		## Return a matrix that is the inverse of 'x'
        
}
