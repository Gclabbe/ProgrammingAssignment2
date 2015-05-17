## cachematrix.R
## Labbe May 17, 2015

## provide cache services to surround the solve function for computing the inverse of a matrix
## For the purpose of this tool, the matrix has to be invertible.  There is no error handling
## at this level.

## Call by setting a variable with the return from passing a matrix to makeCacheMatrix
## Then use cacheSolve instead of solve.  The first call to cacheSolve will take care of calling
## solve.  Subsequent calls will find the cached version and avoid re-running the solve funtion.

## Manage the cached references for the specified matrix
makeCacheMatrix <- function(x = matrix()) {
    ## make sure the solver answer is set to NULL
    s <- NULL
    
    ## push the incoming matrix to the cache and clear any cached solve answer
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## fetch the currently cached matrix
    get <- function() x
    
    ## push the solve answer to the cache
    setInverse <- function(solve) s <<- solve
    
    ## fetch existing solve answer from the cache
    getInverse <- function() s
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Wrapper for solve function that will check cache to see if there is a solution available
cacheSolve <- function(x) {
    ## Attempt to get the inverse from cache.  If it's not there, return is NULL
    s <- x$getInverse()
    
    ## if a cached version was found (i.e. not Null returned), send that back and exit the function
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## if null was returned, there's no cached answer, compute one and then cache it
    message("no cache, running solve()")
    data <- x$get()
    s <- solve(data)
    x$setInverse(s)
    s
}