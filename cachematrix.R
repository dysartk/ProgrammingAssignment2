## makeCacheMatrix does a similar job as the original makeCacheMean function.
## cacheSolve repeats the similarity for solving the inverse of the matrix.
##
## makeCacheMatrix does the same job here as the example code did for the makeVector 
## function. Here a method to hold the value of the inverse of a matrix 
## we create is defined. The return of this function is really list of 4 functions as
## before. Here the functions are re-named to reflect the task they complete.
## 

makeCacheMatrix <- function(x = matrix()) {
	i = NULL
	set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function basically takes the matrix assigned when calling the above function and
## first checks to see if the i contains, NULL, and if so return the value stored. If 
## not then the inverse of the matrix is calculate by assigning the matrix from the x$get
## to data and then assigning the inverse to i.

cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
