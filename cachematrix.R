
## The makeCacheMatrix function creates a special "matrix" object that can cache
## inverse

makeCacheMatrix <- function(x = matrix()) {
  #setting the value of the matrix
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    #getting the value of the matrix
    get <- function() x
    #setting the inverse of the matrix
    setinverse <- function(solve) m <<- solve
    #getting the inverse of the matrix
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix from makeCacheMatrix or if the 
## inverse has already been calculated, then it retrieves the inverse from the  
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #if the matrix inverse is cached
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #if the matrix invers is not cached then calculating its inverse
  m <- solve(data,...)
  x$setinverse(m)
  m
}
