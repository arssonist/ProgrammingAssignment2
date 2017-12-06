## Put comments here that give an overall description of what your
## functions do

# mackCacheMatirx is function that takes a matrix, holds getters and setters that can be called using closures from another function. Hold cache of values within

makeCacheMatrix <- function(matrix = matrix()) {
    # empty cache value- intiliazed object to be used later
    cache <- NULL
    # setter for the matrix input
    #### takes input value- from when setting makeCacheMatrix to a variable This acually creates the matrix itself
    setMatrix <- function(y){
    #### assigns input argument to parent env
        matrix <<- y
    #### cache null before function called- will change when setInverse is called
        cache <<- NULL
    }
    # getter for the value of the matrix input
    getMatrix <- function() matrix
    # setter for the inverse function
    setInverse <- function(toInvert) cache <<- toInvert
    # getter for the inverse function
    getInverse <- function() cache
    ####set all value to list- output is out "special" matrix
    list(set = setMatrix, get = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve calls the previous function with inside getter and setters.
cacheSolve <- function(specialMatrix, ...) {
        # take input special matrix, call getInverse()
        cache <- specialMatrix$getInverse()
        # if there is a cache then call it up
        if(!is.null(cache)){
            message("retrieving cache value")
            return(cache)
        }
        # otherwise- get the previous functions list
        data <- specialMatrix$get()
        # invert it
        cache <- solve(data)
        # user the setter to cache it
        specialMatrix$setInverse(cache)
        # call the cache
        cache
}
# Test Cases

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
myMatrix <- makeCacheMatrix(n1)
cacheSolve(myMatrix)

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix2 <- makeCacheMatrix(m1)
cacheSolve(myMatrix2)

# Tests from post below- proven that function is working https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
