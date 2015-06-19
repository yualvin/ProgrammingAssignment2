## The following functions are derived exactly from the 'cachemean' example 
## given, the only difference being replacing the 'mean' function in the example  
## with the'solve' function to return the inverted matrix

## As per the example, the first function creates a list containing a function
## to set matrix, get matrix, set the inverted matrix and get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)m<<-inverse
        getinverse<-function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The second function calculates the inverted matrix of the matrix created above
## It first checks to see if the inverted matrix is already calculated and gets
## the cache if so. If not, it calculates and returns the inverted matrix via
## the 'setinverse' function

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data)
        x$setinverse(m)
        m
}
