## Below are two functions that are used to create a special object that
## stores a numeric matrix and cache's its inverse.

## This function creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrixr
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        my_Invmatrix <- NULL
        set <- function(y){
                x <<- y
                my_Invmatrix <<- NULL
        }
        get <- function() x
        setInv <- function(inv) my_Invmatrix <<- inv
        getInv <- function() my_Invmatrix
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {
        my_Invmatrix <- z$getInv()
        if(!is.null(my_Invmatrix)){
                message("getting cached data")
                return(my_Invmatrix)
        }
        message("data is set")
        data <- z$get()
        my_Invmatrix <- solve(data, ...)
        z$setInv(my_Invmatrix)
        my_Invmatrix
}
