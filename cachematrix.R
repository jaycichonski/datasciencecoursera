## These functions store and cache the inverse of a matrix. 
## makeCacheMatrix sets up the functions and cacheSolve caches the matrix inverse

## makeCacheMatrix, This function creates a special "matrix" object that can cache its inverse.
## it offers 4 funtions to set and retireve the orginal and inverse of matrices

makeCacheMatrix <- function(x = matrix()) 
{
        # initialize
        matrixInverse <- NULL 
        # set to establish the new matrix
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL 
        }
        
        # get to retrieve the matrix        
        get <- function() x 
        
        # setInv to establish the inverse of the matrix         
        setInv <- function(inverse) matrixInverse <<- inverse 
        
        # getInv to establish the inverse of the matrix        
        getInv <- function() matrixInverse 
        
        # set of functions returned for use      
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## cacheSolve, This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x,...) 
{
        # retrieve the inverse of the matrix, if it exists
        matrixInv <- x$getInv() 
        
        # if it exists return it
        if(!is.null(matrixInv)) { 
                message("retrieving cached data")
                return(matrixInv) 
        }
        
        # else... get the matrix and solve
        matrixInit <- x$get() 
        matrixInv <- solve(matrixInit) 
        
        # set it to the current matrix and return it
        x$setInv(matrixInv)
        return(matrixInv)
}

