## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix through a process
## that includes 4 functions (set and get the matrix, set and get
## the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
                ## this step is used to store the values in 
                ## the parent space. (not inside the set function)
        }
        get <- function() x
        set_inverse <- function(solve) i <<- solve
        ## this steps stores the inverse matrices in the parent
        ## environment, not in the set_inverse() function.
        get_inverse <- function() i
        list(set = set, get = get, set_inverse =set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
## This function creates the inverse of a matrix formed by 
## the makeCacheMatrix function. If first checks whether the
## inverse of the matrix have been calculated before. If it is
## calculated it returns the calculated matrix. If it is not, 
## it creates the inverse of it and returns it. 

cacheSolve <- function(x, ...) {
        function(x, ...){
                i <- x$get_inverse
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$set_inverse(i)
                i
        }
}
