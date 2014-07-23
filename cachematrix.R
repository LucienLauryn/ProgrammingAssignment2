## Put comments here that give an overall description of what your
## functions do

##This function stores the matrix parameters as set and allows retrieval
## using the get function. Additionally, it sets the inverse matrix as the setmatrix
## function when the cacheSolve function is initiated and had a getmatrix function to retrieve it. 

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


##The cacheSolve function checks if the inverse matrix was already stored 
##otherwise it creates the inverse matrix and uses the makeCashmatrix's setmatrix function
## to store the inverse matrix
##

cacheSolve <- function(x, ...) {
	  
	  m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
