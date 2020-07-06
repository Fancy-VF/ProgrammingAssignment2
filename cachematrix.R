# This program gets a matrix and finds the inverse of the matrix

## The makeCacheMatrix function will get a matrix and set the matrix in the global environment and find the inverse of that matrix 
## and save that matrix in the global environment

### The below function has one argument which is the matrix x of order n
print("Enter an invertible square matrix of order n")
makeCacheMatrix <- function(x = matrix(,n)) {
        ### Setting the value of m to NULL
        m<-NULL
        ### Get the value of matrix y and assigns it to x in the global environment
        set<-function(y){
        x <<- y
        m <<- NULL
        }
        ### Getting the value of matrix x using anonymous function
        get<- function() x
        ### Calculates the inverse of matrix x and saving it in m and storing the value in the global environment
        setinverse <- function(solve) m <<- solve(x)
        ### The inverse value is retrived using the getinverse function
        getinverse <- function() m
        ### The list returns four values
        list(set = set, get = get, 
               setinverse = setinverse, getinverse = getinverse)
}


## This function get a matrix and check whether inverse is already calculated
## If yes then returns the value else it will call the function solve to calculate and gives
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- getinverse(x)
        ### The if condition checks whether m is not the NULL matrix, if yes then it returns the value of inverse matrix from cached data 
    if(!is.null(m)){
        message("getting catched data of inverse")
        return(m)
    }
     ### If m is NULL then we solve and get inverse of the matrix and set this value in the global environment for future reference   
    data<- get(x)
    m <- solve(data, ...)
    setinverse(m)
    ### Gives the inverse of matrix
     print("The inverse of given matrix is : ")
        m
}
