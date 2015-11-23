#makeCacheMatrix is a function which creates a special matrix and caches its inverse
#it has a list of functions to set the values of a matrix,get the values of a matrix,set the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  c<- NULL
  #1.function to set the values in a matrix
  set <- function(y) {
    x <<- y
    c<<- NULL
  }
  
  #2.function to get the values of a matrix
  get <- function() x
  #3.function to set the inverse matrix once the inverse is calculated so that the inverse matrix is 
  #stored in the cache and can be used again if needed 
  setinv <- function(inverse) c <<- inverse
  #4.function to get the inverse matrix that is present in the cache
  getinv <- function() c
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#function to calculate the inverse matrix and also to set the inverse matrix in the cache
#it basically checks for the result in the cache, if present, the inverse matrix is returned
#else the inverse matrix is calculated and is stored in the cache
cacheSolve <- function(x, ...) {
  #checking if the inverse matrix exits in the cache via getinv() method
  c <- x$getinv()
  if(!is.null(c)) {
#if present,the inverse matrix is printed
    message("getting cached data")
    return(c)
#if the inverse matrix is not calculated, it is calculated via solve() method and
    #then stored in the cache
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setinv(c)
  c
#the inverse matrix after calculated is printed
}

## Input matrix should be Non singular matrix, which is determinant of matrix is not equal to zero
## sample output
# k<-matrix(1:4,2,2)
# > l<-makeCacheMatrix(k)
# > cacheSolve(l)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(l)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
