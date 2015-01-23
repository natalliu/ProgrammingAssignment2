## The aim of these functions is to avoid potentially time consuming 
##computations of the inverse of a numeric matrix if it need to be 
##computed repeatedly.  Once computed the inverse gets cached and stored in the 
##"special matrix"object together with the matrix itself.
##First call makeCacheMatrix() function to create "special matrix"
##out of the numeric matrix, then call CacheSolve() to calculate or
##retreive its inverse.
#The fuctions work for invertible matrixes only.


## function creates a "special matrix" object, that stores a numeric matrix
##and its inverse if the inverse has already been calculated.
makeCacheMatrix <- function(x = matrix()) {
        ##return list of functions to get/set matrix and its inverse
        i <- NULL
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


##calculate the inverse of the matrix stored in a "special matrix" object or return 
##cached value if it has been already calculated or set.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
