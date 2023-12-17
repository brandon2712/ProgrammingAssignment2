## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#create matrix that can cache inverse

makeCacheMatrix <- function(x = matrix()) {  #create matrix that can cache inverse
  inv <- NULL  #initialize a variable inv
  set <- function(y) {  #assign y to matrix x
    x <<- y
    inv <<- NULL
  }
  get <- function() x   #return current value to matrix x
  setInverse <- function(inverse) inv <<- inverse   #define setInverse
  getInverse <- function() inv   #defind getInverse
  list(set = set,   
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

#compute the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #retrieving inv from matrix x
  if (!is.null(inv))  #check if inv is null, if its not, return inv
    return(inv)
  data <- x$get()  #retrieve matrix data from matrix x
  inv <- solve(data, ...)  #compute data
  x$setinverse(inv)  #set inv into matrix x
  inv
}



mmatrix <- makeCacheMatrix(matrix(1:4,2,2))

mmatrix$get()
mmatrix$getInverse()

cacheSolve(mmatrix)
