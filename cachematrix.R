## Programming Assignment 2

## The following pair of functions caches the inverse of a matrix

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    
    set<-function(y){
        x<<-y
        i<<-NULL
    } 
    get<-function() x
    
    #?
    setInverse<-function(solve) i<<-solve
    
    getInverse<-function() i
    
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix()
## above. If the inverse has already been calculated (and the matrix has not changed),
## cacheSolve() retrieves the inverse from the cache.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getInverse()
    if(!is.null(i)){
        message("Getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setInverse(i)
    i
}

##TEST
W<-matrix(c(1,2,2,1),nrow=2,byrow=TRUE)
makeCacheMatrix(W)
cacheSolve(W)

Y<-matrix(c(1,3,3,2,3,4,3,2,4),nrow=3, byrow=TRUE)
makeCacheMatrix(Y)
cacheSolve(Y)

