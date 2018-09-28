## Put comments here that give an overall description of what your
## functions do
#This program contains functions that cache the inverse of a matrix.
#
# makeCacheMatrix function creates a special "matrix" object that
# can cache its inverse.
# cacheSolve function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated,
# then the cacheSolve will retrieve the inverse from the cache.



# this function creates a 'special' matrix which is a list of functions that sets and gets
#the matrix and sets and gets the inverse after it has been computed
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {       # defining function set
    x <<- y
    inv <<- NULL
  }
  get <- function() x      # getting values from matrix x
  setinverse <- function(inverse) inv <<- solve(x)      #setting inverse of the matrix in inv
  getinverse <- function() inv       # getting inverse of the matrix
  list(set = set, get = get,       # defining a list of the above 4 functions and naming them
       setinverse = setinverse,
       getinverse = getinverse)
}



# this fucntion calculates the inverse of a matrix by using the list of functions defined above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()                #applying getinverse and storing value in inv
  if(!is.null(inv)) {                   # checking if inverse has been calculated
    message("getting cached result")
    return(inv)
  }
  data <- x$get()        #collecting matrix data
  inv <- solve(data)    #finding inverse of the matrix data collected above using solve function and storing it in inv
  x$setinverse(inv)       #setting value of inverse in a cache using setinverse function on matrix x
  inv
}
