## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {#function to create a special matrix object that can cache its inverse
 inv <- NULL  #initializing inverse as NULL
  set <- function(y) { #calling a set function with y as an argument
    x <<- y #assigning y to object x
    inv <<- NULL #assign NULL to inv (which is already pre-defined outside of the set function)
}
get <- function() x #function to get matrix x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv #function to obtain inverse of the matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){ #checking if inverse is null. 
                     #if inverse has been calculated, don't recompute.
                     # get it from the cache and print the message 
    message("getting cached data")
    return(inv) #returns inverse value
   }
  data_matrix <- x$get() 
  inv <- solve(data_matrix, ...) #compute the inverse value
  x$setinverse(inv) #set the inverse value in the cache via the set(inv) function
  inv #return a matrix that is inverse of x
}

