## This function creates a special "matrix" object saves this in a cache, and then retrieve it from the cache.

makeCacheMatrix <- function(x = matrix()) {
       inverse_var<- NULL
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setinv <- function(inverse) inverse_var <<- inverse
        getinv <- function() inverse_var
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse_var = x$getinv()
        if (!is.null(inverse_var)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inverse_var)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inverse_var)
        
        return(inverse_var)
}
