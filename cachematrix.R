## Function to cache an inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    
    set <- function (y) {
      x <<- y
      invr <<- NULL
    }
    get <- function() x
    
    # use `<<-` to assign a value in diff environment
    setInvMtx <- function(solve) invr <<- solve
    getInvMtx <- function() invr
    
    
    list (set = set, 
          get = get,
          setInvMtx = setInvMtx,
          getInvMtx = getInvMtx)
        
}


## Function to execute the inverse of a Matrix
## created using makeCacheMatrix funv
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invr <- x$getInvMtx()
    
    if (!is.null(invr)) {
        #Value if found in cache
        message("getting cached data")
        return(invr)
    }
    #If not in cache then executes the inverse and stores it in cache
    rs <- x$get()
    invr <- solve(rs, ...)
    
    x$setInvMtx(invr)
    
    return(invr)
}
