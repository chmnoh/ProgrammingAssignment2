## Caching the Inverse of a Matrix
## 


## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ci <- NULL # cached inverse
    set <- function(y) {
        x <<- y
        ci <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inv) ci <<- inv
    getInverse <- function() ci
    list(set=set, get=get, 
         setInverse = setInverse,
         getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
## retrieve the inverse from the cache if the matrix not changed
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,identityMatrix(ncol(data)),...)
    x$setInverse(inv)
    inv
}

# create n x n identity matrix
identityMatrix <- function(n) {
    m <- matrix(rep(0,n*n),n,n)
    for (i in 1:n) {
        m[i,i] <- 1
    }
    m
}
