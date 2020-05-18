## Put comments here that give an overall description of what your
## functions do


## creates special matrix for caching inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getInv <- function() inv
    setInv <- function(i) inv <<- i
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    
    
}


## returns inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInv(inv)
    inv
}
