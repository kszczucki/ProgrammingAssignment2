## Caching the Inverse of a Matrix

## makeCacheMatrix Calculates the inverse of given matrix. If the inverse has already been calculated 
#it gets it from the cache and skips the computation. Otherwise, it calculates the inverse of the data 
#and sets the value of the inverse in the cache via the setinverse function.

makeCacheMatrix <- function(x = numeric()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){
                x
        } 
        setinverse <- function(solve) {
                inv <<- solve
        }
        getinverse <- function(){
                inv
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x,...){
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}