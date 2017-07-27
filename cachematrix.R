## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Creating a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #Setting value of the matrix object
        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        #Getting value of the matrix
        
        get <- function() x
        
        #Setting value of matrix inverse
        setinverse <- function(solve) inv <<- solve
        
        #Getting value of matrix inverse
        getinverse <- function() inv
        
        #List of functions to be drawn from in cacheSolve
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function

##Computing inverse of special matrix from makeCacheMatrix. If already 
#calculated, inverse should be retrieved from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() #retrieves inverse matrix value
        if(!is.null(inv)) {  #fetches matrix from cache if already calculated
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #fetches matrix if conditional not met
        inv <- solve(data, ...) #calculates inverse
        x$setinverse(inv) #sets inverse
        inv
}

##returns 4x4 matrix when x <- matrix(sample(16), 4, 4)*matrix(sample(16), 4, 4)