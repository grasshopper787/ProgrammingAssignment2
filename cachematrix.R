##  These functions creates a special "matrix" object that can cache its inverse (makeCacheMatrix).
##  and computes the inverse of the special "matrix" returned by makeCacheMatrix above (cacheSolve).
##  If the inverse has already been calculated (and the matrix has not changed),
##  then cacheSolve should retrieve the inverse from the cache.

##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  The first function, makeCacheMatrix creates a special "matrix",
##  which is really a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##  The second function, cacheSolve computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been calculated
##  (and the matrix has not changed), then the cacheSolve should retrieve 
##  the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}

##  Test of finctions
##  x <- rbind(c(1,1/2),c(-1/2,1)) 
##  m <- makeCacheMatrix(x)
##  m$get()
##  cacheSolve computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above
##  cacheSolve(m)
##  The inverse has already been calculated
##  (and the matrix has not changed).
##  cacheSolve(m)
