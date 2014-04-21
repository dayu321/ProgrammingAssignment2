## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix contains a matrix and cache its inverse version
makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMatrix contains a matrix and its inverse version
    ## user can access both of them via cacheSolve function
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


## cacheSolve computes the composite data structure created by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## cacheSolve computes the composite data structure created by makeCacheMatrix
    ## once the inverse of matrix is computed, it will automatically 
    ## cache it into the struture of makeCacheMatrix
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


# test case:
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
