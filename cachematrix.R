## makeCacheMatrix function creates a list containing fuctions 
## for setting and getting values of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	## By default inverse is set to be NULL
	inv <- NULL
	
	## Functions for setting and getting original matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	
	## Functions for setting and getting matrix inverse
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	
	## The makeCacheMatrix function finally returns list of set and get functions
	list (set = set, get = get, setinv = setinv, getinv = getinv)		
}


## cacheSolve function returns matrix inverse,
## preliminary checks if it already exists in cache

cacheSolve <- function(x, ...) {

	## Check if inverse already exists. If yes - inverse is taken from cache and returned 
	inv <- x$getinv()
	if (!is.null(inv)) return(inv)

	## If inverse doesn't exist - it's calculated, set to cache for future use and returned
	inv <- solve(x$get(), ...)
	x$setinv(inv)
	inv
	
}
