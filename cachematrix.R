## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinv <- function(Inverse) inv <<- Inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}


a <-matrix(c(0, 1, 3, 3, -1, -1, 1, 1, 2), 3, 3)
b <- makeCacheMatrix(a)
cacheSolve(b)
#      [,1] [,2] [,3]
# [1,] -0.2 -1.4  0.8
# [2,]  0.2 -0.6  0.2
# [3,]  0.4  1.8 -0.6
cacheSolve(b)
# getting cached data
#      [,1] [,2] [,3]
# [1,] -0.2 -1.4  0.8
# [2,]  0.2 -0.6  0.2
# [3,]  0.4  1.8 -0.6
