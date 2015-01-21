## This program is used to calculate matrix inverse and store it into cache
## If the inverse of the same matrix is required again, it is not calculated
## Instead it is fetched from the cached value, thus requiring less time

## This function creates a special matrix which has the ability to cache 
## it's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function takes a special matrix as input and checks if it the inverse
## of the matrix already exists or not. If the invers exists in cache, then it 
## is retrieved. Else, the inverse is calculated and the cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
