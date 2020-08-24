## A pair of function to store special object matrix and compute the inverse of the matrix

## This function is used to create a special matrix object which will store the 
## matrix and cache the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
     x <<- y
     i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function will compute the inverse of the special matrix object from the 
## makeCacheMatrix function output

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Testing the function if it will inverse the matrix we assigned here
## NOTE matrix supplied is always invertible
test = matrix(c(10,2,4,5),3,3)

## first make the cache matrix
a = makeCacheMatrix(test)

## now obtain the inverse of the matrix by using this function
b = cacheSolve(a)

## From the object b , we have succesfully inverse the matrix
