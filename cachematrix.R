## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL  
        
        set <- function(y) {           
                x <<- y               
               inv <<- NULL            
       }
      
        get <- function() x 
        setinv <- function(inverse) inv <<- inverse  
        getinv <- function() inv

       list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the special "matrix". However, 
## it first checks to see if the inverse has already been calculated. If so,
## it obtains the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse via the setInverse() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() 
	#if the inverse has already been calculated 
       if (!is.null(inv)){
              message("Getting cached data")
              return(inv)
       }
       
	#otherwise, calculates the inverse 
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinv(inv)  
        return(inv)
}
