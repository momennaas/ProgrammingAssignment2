##Two functions one to cache the inverse of a matrix and another funciton to return 
##the return the inverse of a function.

##makeCacheMatrix creates a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y){
        x <<- y
        m <- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(solve) {
        m <<- solve
    }
    
    getInverse <- function() {
        m
    }
    
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


##cacheSolve returns matrix inverse if the matrix  is already computed cached otherwise
##caches the matrix and compute its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    if(!is.null(m)){
        message("Getting cached inverse")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    
}