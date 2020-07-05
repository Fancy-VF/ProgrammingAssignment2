## This program gets a matrix and finds the inverse of the matrix

## The makeCacheMatrix function will get a matrix and set the matrix in the global environment and find the inverse of that matrix 
## and save that matrix in the global environment

makeCacheMatrix <- function(x = matrix(,n)) {
        m<-NULL
        set<-function(y){
        x <<- y
        m <<- NULL
        }
        get<- function() x
        setinverse <- function(solve) m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get, 
               setinverse = setinverse, getinverse = getinverse)
}


## This function get a matrix and check whether inverse is already calculated
## If yes then returns the value else it will call the function solve to calculate and gives
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- getinverse(x)
    if(!is.null(m)){
        message("getting catched data of inverse")
        return(m)
    }
    data<- get(x)
    m <- solve(data, ...)
    setinverse(m)
    m
}
