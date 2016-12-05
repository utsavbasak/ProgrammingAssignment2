## There are two functions, one to create a vector that stores the reference 
## to the functions to set, get, setinverse and get inverse. This function
## stores the values in the same vector that holds the functions.
## The second functions checks if the inverse value is already there and it then
## either retrieves it from the cache or generates the inverse. It takes as 
## input the type of vector formed from the preceding operation

## This function is to create a vector which essentually stores other functions 
## to set, get, getinverse, setinverse of a matrix. It also stores the values of
## the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inver <<- solve
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function takes as input a vector of the type specified 
## above(makecachematrix), and gives as output an inverse if there is a new 
## matrix, or gets the cache value of inverse if it already exists. It checks
## to see if the value exists within the if block, and retrieves or generates the
##inverse

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
        ## Return a matrix that is the inverse of 'x'
}
