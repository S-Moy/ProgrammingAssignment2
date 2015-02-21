##
##The functions makeCacheMatrix and cacheSolve were created to 
##produce a method for calculating the inverse of matrix and 
##storing the result in cache in another environment.
##
##



##
##makeCacheMatrix creates a special "matrix" object that caches its inverse.
## 
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



##
##cacheSolve: This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.
##

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getinv()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinv(m)
    m
}
