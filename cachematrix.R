## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Creating a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #Setting value of the matrix
        inv <- NULL
        set <- function(y = as.matrix()) {
                x <<- y
                inv <<- NULL
        }
        #Getting value of the matrix
        get <- function() x
        #Setting value of matrix inverse
        setinverse <- function(solve) inv <<- solve
        #Getting value of matrix inverse
        getinverse <- function() inv
        #Creation of special matrix containing function to cache inverse
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function

##Computing inverse of special matrix from makeCacheMatrix. If already 
#calculated, inverse should be retrieved from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
