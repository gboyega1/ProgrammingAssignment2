## makeCacheMatrix is a function that initializes two objects; x: a matrix
## and i: a null matrix which is to be the inverse of x then creates functions
## responsible for allotting and retrieving values for both x and i. 
## makeCacheMatrix then creates a new object by placing these functions into
## a list. This list is to serve as input into the function cacheSolve.
## CacheSolve is a function which takes as input, an object x of data type 
## makeCacheMatrix, retrieves the matrix defined in makeCacheMatrix
## calculates it's inverse and returns the inverse as output

## makeCacheMatrix takes a matrix as input argument, defines functions for
## defining and retrieving its value and inverse and stores these functions
## within a list

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y){
x<<-y
i<<-NULL
}
get<-function()x
setinv<-function(inverse)i<<-inverse
getinv<-function()i
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve takes as input, x, an object of type makeCacheMatrix, checks
## if i defined in makeCacheMatrix is NULL; if no, returns its value,
## but if yes, retrieves the value of x defined in makeCacheMatrix and uses it
## to compute the value of i, it's inverse and
## returns i as output

cacheSolve <- function(x, ...) {
i<-x$getinv()
if(!is.null(i)){
message("getting cached matrix")
return(i)
}
data<-x$get()
i<-solve(data,...)
x$setinv(i)
i ## Return a matrix that is the inverse of 'x'
}
