## Functions to cache the inverse of a non-singular matrix

## This function creates a special "matrix" object which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # initializing to NULL
        invm <- NULL
        set <- function(y){
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) invm <<- solve
        getmatrix <- function() invm
        list(set=set, get=get,
                setmatrix=setmatrix,
                getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invm <- x$getmatrix()
        
        ## Check if the inverse exists
        if(!is.null(invm)){
               message("getting cached data")
               return(invm)
        }
        
        #if the inverse if not there, calculating the inverse
        matx <- x$get()
        invm <- solve(matx, ...)
        x$setmatrix(invm)
        invm
}
