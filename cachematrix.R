## Programming Assignment-2 
## 1) Take advantage of the scoping rules of the R language
## to Cache variables. Use it to cache a matrix inverse result
## 2) use <<- operator to set variable value outside the 
## scope of function

## Function name: makeCacheMatrix
## This function creates a special "matrix" object
## Input : A invertible matrix. for e.g.
## mymat <- 2 * diag(3) 
## testm <- makeCacheMatrix(mymat)
## 
## it has 4 functions for the matrix object 
## 1) set() - set the value of matrix
## 2) get() - get the value of matrix. for e.g. testm$get()
## 4) setinverse() - set the value of inverse
## 3) getinverse() - get the value of inverse for e.g. testm$getinverse()
makeCacheMatrix <- function(x = matrix()) {
            #invm is the variable to store the inverse
            invm <- NULL
			
            set <- function(y) {
                    x <<- y
                    invm <<- NULL
            }
            get <- function() x
			##set the value of inverse
            setinverse <- function(inverse) invm <<- inverse
			
			## get the value of inverse
            getinverse <- function() invm
			
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Function name: cacheSolve
## checks if matrix inverse is already computed
## if not then, computes the inverse and returns the result
## else returns the already cached matrix inverse
##
## Input : matrix object created using makeCacheMatrix()
## for e.g. 
## mymat <- 2 * diag(3) 
## testm <- makeCacheMatrix(mymat)
## testminv <- cacheSolve(testm)
## Return: return the inverse of matrix
##
## Also prints "getting cached matrix" if it is 
## returning a cached matrix. No message is
## printed if fresh inverse computation is done.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 
		## call getinverse from makeCacheMatrix to check if
		## matrix is already computed and cached
		invm <- x$getinverse()
            if(!is.null(invm)) {
                    message("getting cached matrix")
                    return(invm)
            }
		
		## get the matrix to take inverse
        data <- x$get()
		
		##compute matrix inverse using solve() in-built function
        invm <- solve(data, ...)
		
		## set the inverse value 
        x$setinverse(invm)
		
		##return inverse
        invm
}