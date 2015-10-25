## Take advantage of the scoping rules of the R language
## to preserve state inside of an R object.
## <<- operator is used to set variable value outside the 
## scope of function

## This function creates a special "matrix" object
## whose inverse can be computed
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## checks if matrix inverse is not already computed
# if not function computes the inverse
# else returns the cached matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
