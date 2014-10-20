#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #var m is declared uniquely in both functions and are allocated sepearate addresses in memory.
    m <- NULL 
    #set
    set <- function(y) {
        #superassignment operator: assign the value "y" to variable 'x' declared outside this function
        x <<- y 
        m <<- NULL
    }
    #get (return x)
    get <- function() x
    #set the inverse of a matrix to variable 'm'
    setinverse <- function(inverse) m <<- inverse
    #get the inverse of a matrix
    getinverse <- function() m
    #create a list of functions to work with the (inverse) matrix
    list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
    #read the inverse matrix
    m <- x$getinverse() 
    #If the inverse has already been calculated (and the matrix has not changed), 
    #then the cachesolve should retrieve the inverse from the cache.
    if(!is.null(m)) {      
        message("getting cached data")
        return(m)
    }
    #otherwise, (could sit in else{}) create inverse of a matrix
    #solve computes the inverse of the matrix from x$get()
    m <- solve(x$get()) 
    #save the matrix in environment
    x$setinverse(m) 
    #print
    return(m) 
}
