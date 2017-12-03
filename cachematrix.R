## Programming Assignment 2

## The following pair of functions caches the inverse of a matrix

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    } 
    get<-function() x
    setsolve<-function(solve) inv<<-solve
    getsolve<-function() inv
    list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}

## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix()
## above. If the inverse has already been calculated (and the matrix has not changed),
## cacheSolve() retrieves the inverse from the cache.

cacheSolve <- function(x,...) {
## Return a matrix that is the inverse of 'x'
    inv<-x$getsolve()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data)
    x$setsolve(inv)
    inv
}


## Matrices for testing the R functions 

##t1
a<-matrix(c(1,2,2,1),nrow=2,byrow=TRUE)
a1<-makeCacheMatrix(a)
cacheSolve(a1)
##         [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

##t2
b<-matrix(c(1,3,3,2,3,4,3,2,4),nrow=3, byrow=TRUE)
b1<-makeCacheMatrix(b)
cacheSolve(b1)
##        [,1] [,2] [,3]
## [1,]    4   -6    3
## [2,]    4   -5    2
## [3,]   -5    7   -3

##t3
c <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
c1<-makeCacheMatrix(c)
cacheSolve(c1)
##        [,1] [,2]
## [1,]    6    8
## [2,]    2    4







