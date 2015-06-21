## Creating a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	set <- function(matrix) {
		x <<- matrix
		i <<- NULL
	}

	get <- function() {
		x
	}

	setInverse <- function (inverse) {
		i <<- inverse
	}

	getInverse <- function() {
		i	
	}
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## 

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	x <- z$getInverse()

	if(!is.null(x)) {
		message("getting cached data")
		return(x)
	}
	
	data <- z$get()
	x <- solve(data) %*% data
	z$setInverse(x)
	x
}
