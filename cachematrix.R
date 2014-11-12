## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( x = matrix()) {
        ## Creates variable in local environment
        i <- NULL
        set <- function(y) {
                ## Supperassigns value of y to x, where x is makeVector()'s input
               x <<- y
               i <<- NULL
        }
        ## This can be called to retrieve the values of x from makeVector
        get <- function()  x
        ## This sets i to solve in the parent env (makeVector())
        setinverse <- function(solve)  i <<- solve
        ## R will look for the value of i in getinverse()
        getinverse <- function()  i
        ## This is needed to make functions accessible
        list(set = set, get = get, setinverse = setinverse,
             getinverse= getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function( x, ...) {
        ## This calls the getinverse() function from x. x is a container environment that
        i <-  x$ getinverse()
        if(!is.null( i)) {
                message("getting cached data")
                return( i)
        }
        ## Calls get() function from x, which won't have an i value
        data <-  x$get()
        i <- solve(data, ...)
        ## With the new solve, setinverse() will be called to to update i in makeVector()
        x$ setinverse( i)
        i
}
