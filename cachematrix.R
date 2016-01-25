#### ASSIGNMENT 2. WEEK 3. R PROGRAMMING.

## This is the function that will create the cache matrix.

makeCacheMatrix <- function(x = matrix()) {
        #This next lines set the values of the objects in the environments 
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x #This gets the value of the vector
        setinverse <- function(inverse) matinv <<- inverse #This will set the value of the inverse matrix
        getinverse <- function() matinv #This will get the value of the inverse matrix
        #this is the return of the function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the matrix inversion, first it checks it the inversion has been computed with the "..." argument

cacheSolve <- function(x, ...) {
        matinv <- x$getinverse() #if it finds the inversion calculation it gets it from the cache.
        if(!is.null(matinv)) {
                message ("getting cached data")
                return(matinv)
        }
        #If it doesn't get the inversion it will inverted and set it.
        mat <- x$get()
        matinv <- solve(mat, ...)
        x$setinverse(matinv)
        matinv
}
