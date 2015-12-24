##  These functions create a 'special' matrix which has four built-in functions
##  (set,get,setinverse and getinverse) and remembers of its inverse after it
##  has been evaluated once.
## 
##  To use:
##      Create a special 'matrix' by passing an ordinary (invertible) matrix to
##      makeCacheMatrix() and assigning the reurn value to a variable.
##      E.g.
##          m <- matrix(c(1,1,1,0,2,0,0,0,3),nrow=3)
##          s <- makeCacheMatrix(m)
##      
##      You can now get the inverse of the matrix
##      E.g.
##          i <- cacheSolve(s)
##      Subsequent calls to cacheSolve for the same 'matrix' will return the
##      inverse without recalculating.
##
##  Note: You can also call these functions directly using the $ notation
##      set(x) - change the 'matrix' to a new value
##      get() - get the 'matrix' as an ordinary matrix
##      setinverse(x) - set the value of the inverse - NOT RECOMMENDED
##      getinverse() - get the value if the inverse - NOT RECOMMENDED



##  Function to create a new special 'matrix' that will remember its inverse
##  The 'matrix' is really a list containing functions.

makeCacheMatrix <- function(x = matrix()) {

    # initialize the inverse
    
    i <- NULL

    # define the four functions in the context
    # of the x and i variables
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    
    # return the four functions
    
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}




## Calculate the inverse of the special 'matrix' created with the above function.
## Subsequent calls to cacheSolve will return the inverse without recalculating.

cacheSolve <- function(x, ...) {
    
    ## return the cached inverse if it has been set
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if no cached inverse yet, calculate the inverse, cache it, then return it
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
