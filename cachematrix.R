## Aim: cache the inverse of a matric (saves time by not repeatedly computing a matrix)

## Description: 
# 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its 
# inverse. 
# 2.cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
# not changed), then cacheSolve should retrieve the inverse from the cache.

## Example:
# mat <- matrix(c(5,2,1,6),nrow = 2,ncol = 2)
# mat2 = makeCacheMatrix(mat)
# > cacheSolve(mat2)            # first time: compute the inverse
# [,1]        [,2]
# [1,]  0.21428571 -0.03571429
# [2,] -0.07142857  0.17857143
# > cacheSolve(mat2)            # second time: retrieve the inverse from the cache
# getting cached data           
# [,1]        [,2]
# [1,]  0.21428571 -0.03571429
# [2,] -0.07142857  0.17857143

        

## The makeCacheMatrix function creates a list object containing four functions which:
# (i) set the value of the matrix, (ii) get the value of the matrix, (iii) set the value 
# of the matrix inverse, and (iv) get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function first checks if the matrix inverse has been calculated.
# If so, it retrieves the previously calculated (i.e. cached) value and returns this value.
# If not, it calculates the matrix inverse and stores this value using the "setinverse" function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {                     # check if the inverse matrix has been calculated
                message("getting cached data")  # if yes, give a message
                return(inv)                     # return the inverse matrix and end here 
        }
        data <- x$get()                         # if no, first, get the matrix
        inv <- solve(data, ...)                 # second, calculate the inverse matrix
        x$setinverse(inv)                       # cache the inverse matrix
        inv
}

