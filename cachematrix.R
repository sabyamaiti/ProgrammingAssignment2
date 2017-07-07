## Functions to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
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
        if(!is.null(invm)){
                message("getting cached data")
                return(invm)
        }
        matx <- x$get()
        invm <- solve(matx, ...)
        x$setmatrix(invm)
        invm
}
