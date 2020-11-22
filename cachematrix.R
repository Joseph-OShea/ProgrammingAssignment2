#Randall O'Shea, 11/22/2020
#These functions will: 
#1. Create a matrix using input from the user in terms of the size and 
#contents of the matrix, and
#2. Check if the inverse of the matrix has been created and stored in
#a cache.  If so, it will get the result form the cache. Otherwise
#it will compute the inverse of the matrix created using formula 1.

#This function is essentially a list containing functions to set and
#get the values of the matrix, and set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  inversed <- NULL
  set <- function(y) {
    x <<- y 
    inversed <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) {inversed <<- inverse}
  getInverse <- function() inversed
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#The following function will first check if the inverse matrix has 
#been stored, and print it if it has.  If the inverse matrix is NULL,
#then it will first compute the inverse, and then store and print it.

#TIP: although the homework does say to assume that the matrix supplied 
#is always invertible, if the user creates the matrix with a random 
#set of numbers, by using SAMPLE in the code when the matrix is created,
#it will be more likely that an invertible matrix is created.
#EX: userMatrix <- makeCacheMatrix(matrix(sample(1:16), nrow = 4, ncol = 4)).

cacheSolve <- function(x, ...){
  inversed <- x$getInverse()
  if(!is.null(inversed)){
    message("getting cached data")
    return(inversed)
  }
  matrix <- x$get()
  inversed <- solve(matrix, ...)
  x$setInverse(inversed)
  inversed
}
