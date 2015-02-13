## Two methods, one which creates a list of functions for managing a matrix and its reverse and the other method for lazy-solving the matrix

## Creates a list of functions for managing a matrix and its reverse

makeCacheMatrix <- function(x = matrix()) {
	r <- NULL
	set <- function(y) {
		x<<-y
		r<<-null
	}
	get <- function() x
	setReverse <- function(reverse) r <<- reverse
	getReverse <- function() r
	list (set = set, 
		get = get, 
		setReverse = setReverse, 
		getReverse = getReverse)
}


## if the matrix is already solved then it uses the existing solution otherwise
## it calculates the reverse of the matrix and caches it for future use

cacheSolve <- function(x, ...) {
        r <- x$getReverse()
        if (!is.null(r)) {
        	message ("getting cached reverse")
        	return(r)
        }
        matrix <- x$get()
        r <- solve(matrix)
        x$setReverse(r)
        r
}
