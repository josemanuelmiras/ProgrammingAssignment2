## The two functions written here provide a means for storing a matrix in the cache and obtain its inverse.

## This function creates a special 'matrix' object that can cache its inverse.
## Eventually, this could save time when requirements in computation are high.

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y){
                       x <<- y
                       i <<- NULL
                       }
                       get <- function()x
                       setinverse <- function(inverse) i <<- inverse
                       getinverse <- function() i
                                  list(set = set,
                                       get = get,
                                       setinverse = setinverse,
                                       getinverse = getinverse)
}


## This function computes the inverse of the special 'matrix' created by MakeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then it should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
               if(!is.null(i)){
                                 message("Getting data from cache")
                                 return(i)
                                 }
               mat <- x$get()
               i <- solve(mat, ...)
               x$setinverse(i)
               i        
}
