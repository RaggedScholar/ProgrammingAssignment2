## makeCacheMatrix creates a matrix object with the properties get/set and
## get/set inverse.

## Creatures a special "matrix" object

makeCacheMatrix <- function(x = numeric()) {
    x = as.matrix(x) ## coerce the data into a matrix
    if(dim(x)[1] != dim(x)[2]){
      message("Warning: Matrix is not square, inverse does not exist...")
      message("\'cacheMatrix\' object created anyway, be careful")
    }
    i <- NULL  ## i = inverse
    set <- function(y){
      x <<- as.matrix(y)  ## coerce the data into a matrix
      if(dim(x)[1] != dim(x)[2]){
        message("Warning: Matrix is not square, inverse does not exist...")
        message("cacheMatrix object created anyway, be careful")
      }
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Solves for the inverse of a cacheMatrix, unless the inverse has been
## already created and stored, then just returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)){
      message("Getting the cached inverse...")
      return(i)
    }
    data <- x$get()
    i = solve(data)
    x$setinv(i)
    i
    
}
