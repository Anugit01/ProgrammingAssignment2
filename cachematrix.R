# here is an illustration of how this works when used in R .
#  Amatrix <- makeCacheMatrix(matrix(c ( 1, 2, 2, 1 ), nrow=2, byrow=TRUE))
# Amatrix$get()
# cacheSolve(Amatrix)
# Amatrix$getInverse()
#Amatrix$set(matrix(c ( 1,0,4,1,3,4,4,1,0 ), nrow=3, byrow=TRUE))
# cacheSolve(Amatrix)

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        mInverse <- NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) mInverse <<- inverse
        getInverse <- function() mInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
# retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse of matrix")
                return(inverse)
        }
        m <- x$get()
        inverse <- solve(m)
        x$setInverse(inverse)
        inverse
}
