## Together these functions will find the inverse of a matrix either by 
## calculating it or using the cached value of the inverse 

## makeCacheMatrix creates a special matrix object which will
## set the value of the matrix and get the value of the matrix, 
## set the value of the inverse and get the value of the inverse.
## Creates the input for cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
         x<<-y
         inv<<-NULL
        }
get<-function() x
setinv<-function(inverse) inv <<- inverse
getinv<-function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cachesolve will calculate the inverse of the matrix created by the makeCacheMatrix function
## either by using the cached value or calculating the value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        return(inv)
}
