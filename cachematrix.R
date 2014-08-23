## This 2 functions demonstrate how to cache the inverse of a matrix

## This function creates a matrix object with methods setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL					#initialize cache to store inverse matrix
        set <- function(y) {
                x <<- matrix(1:9
                m <<- NULL
        }
        get <- function() x			#returns the value of the original vector
		
		# this is called by cacheSolve() during the first cacheSolve() access and it will store the value using superassignment
        setinv <- function(inv) inv <<- inverse		

		# this will return the cached value to cacheSolve() on subsequent accesses
        getinv <- function() inv

		##This list is returned with the newly created object.
		##It lists all the functions ("methods") that are part of the object.  
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function returns the inverse of a matrix (which is a matrix itself).
## Calculation is only done if it is not yet available in the cache.
cacheSolve <- function(x, ...) {
		inv <- x$getinv()			#accesses the object 'x' and gets the value of the solve
        if(!is.null(inv)) {			# if inverse was already cached (not NULL) it is returned
                message("getting cached data")
                return(inv)
        }
        data <- x$get()	
        inv <- solve(data, ...)		#inverse is calculated
        x$setinv(inv)				#inverse matrix is stored ...
        inv							#... and returned
}
