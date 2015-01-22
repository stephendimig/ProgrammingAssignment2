#################################################################
##
## File:   cachematrix.R
## Author: Stephen Dimig
## Description:
## Matrix inversion is usually a costly computation and their may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. To accomplish this, this file contains
## a pair of functions that cache the inverse of a matrix.
##    makeCacheMatrix: This function creates a special "matrix" 
##                     object that can cache its inverse. This 
##                     method is very similar to a c++ constructor.
##       
##    cacheSolve: This function computes the inverse of the special
##                "matrix" returned by makeCacheMatrix above. If 
##                 the inverse has already been calculated (and the 
##                 matrix has not changed), then the cachesolve 
##                 will retrieve the inverse from the cache.
##
#################################################################


#################################################################
##
## Description:
## This method is a constructor for a CacheMatrix object that 
## contains the data and methods required to solve a matrix
## for it's inverse and store the result in a cache.
##
## Parameters:
## x - This parameter is the matrix to invert. It will default to
## an empty matrix.
##
## Return:
## This method returns a list that sets up the methods and data
## for a CacheMatrix object. The resulting object can be passed to
## the cacheSolve() method to either solve the matrix or cache 
## the solved value.
##
#################################################################
makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#################################################################
##
## Description:
## This method either solves a matrix for it's inverse or returns 
## a cached value that was previously solved for.
##
## Parameters:
## this - This parameter is the instance of a CacheMatrix object
## created from the call to makeCacheMatrix().
## ... - Any extra parameters passed to the solve() method.
##
## Return:
## This method returns the inverse of the matrix passed in to 
## makeCacheMatrix() either by doing the calculation or from a 
## value cached from a previous solve.
##
#################################################################
cacheSolve <- function(this, ...) 
{
    m <- this$getinverse()
    if(!is.null(m)) {
        return(m)
    }
    data <- this$get()
    m <- solve(data, ...)
    this$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}

#################################################################
##
## Description:
## This method executes unit tests for the CacheMatrix object.
##
## Parameters:
## None
##
## Return:
## A value of TRUE is returned if all tests pass, false is 
## returned otherwise.
##
#################################################################
testCacheSolve <- function()
{
    # Test with matrix #1. See if calculated value matches the
    # expected value.
    identity = rbind(c(1, 0), c(0, 1))
    expected <- rbind(c(-1, 1.5), c(1, -1))
    d <- rbind(c(2, 3), c(2, 2))
    obj1 <- makeCacheMatrix(d)
    matrix <- cacheSolve(obj1)
    if(FALSE == identical(matrix, expected))
    {
        print("Error: test failed due to unexpected result: 1")
        return(FALSE)
    }
    
    # Test that the original multiplied by the result is the 
    # identity matrix.
    prod <- d %*% matrix
    if(FALSE == identical(identity, prod))
    {
        print("Error: test failed due to unexpected result: 2")
        return(FALSE)
    }
    
    # Test cached value
    matrix1 <- cacheSolve(obj1)
    if(FALSE == identical(matrix1, matrix))
    {
        print("Error: test failed due to unexpected result: 3")
        return(FALSE)
    }
    
    # Test with matrix #2. See if calculated value matches the
    # expected value.
    identity <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
    expected <- rbind(c(1, 0, 0), c(0, 1, 0), c(4, 0, 1))
    e <- rbind(c(1, 0, 0), c(0, 1, 0), c(-4, 0, 1))
    obj2 <- makeCacheMatrix(e)
    matrix <- cacheSolve(obj2)
    if(FALSE == identical(matrix, expected))
    {
        print("Error: test failed due to unexpected result: 4")
        return(FALSE)
    }
    
    # Test that the original multiplied by the result is the 
    # identity matrix.
    prod <- e %*% matrix
    if(FALSE == identical(identity, prod))
    {
        print("Error: test failed due to unexpected result: 5")
        return(FALSE)
    }
    
    # Test cached value
    matrix1 <- cacheSolve(obj2)
    if(FALSE == identical(matrix1, matrix))
    {
        print("Error: test failed due to unexpected result: 6")
        return(FALSE)
    }
    
    # Test set method.
    obj2$set(d)
    identity <- rbind(c(1, 0), c(0, 1))
    expected <- rbind(c(-1, 1.5), c(1, -1))
    matrix <- cacheSolve(obj2)
    if(FALSE == identical(matrix, expected))
    {
        print("Error: test failed due to unexpected result: 7")
        return(FALSE)
    }
    
    # Test that the original multiplied by the result is the 
    # identity matrix.
    prod <- d %*% matrix
    if(FALSE == identical(identity, prod))
    {
        print("Error: test failed due to unexpected result: 8")
        return(FALSE)
    }
    
    # Test set method.
    obj2$set(e)
    identity <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
    expected <- rbind(c(1, 0, 0), c(0, 1, 0), c(4, 0, 1))
    matrix <- cacheSolve(obj2)
    if(FALSE == identical(matrix, expected))
    {
        print("Error: test failed due to unexpected result: 9")
        return(FALSE)
    }
    
    # Test that the original multiplied by the result is the 
    # identity matrix.
    prod <- e %*% matrix
    if(FALSE == identical(identity, prod))
    {
        print("Error: test failed due to unexpected result: 10")
        return(FALSE)
    }
    
    # Test the get() method.
    f <- obj2$get()
    if(FALSE == identical(e, f))
    {
        print("Error: test failed due to unexpected result: 11")
        return(FALSE)
    }
    
    #Test setinverse() and getinverse() methods method
    obj2$setinverse(NA)
    matrix2 <- obj2$getinverse()
    if(FALSE == is.na(matrix2))
    {
        print("Error: test failed due to unexpected result: 12")
        return(FALSE)
    }
    
    # Test recovery rom NA value
    identity <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
    expected <- rbind(c(1, 0, 0), c(0, 1, 0), c(4, 0, 1))
    obj2 <- makeCacheMatrix(e)
    matrix <- cacheSolve(obj2)
    if(FALSE == identical(matrix, expected))
    {
        print("Error: test failed due to unexpected result: 13")
        return(FALSE)
    }
    
    # Test that the original multiplied by the result is the 
    # identity matrix.
    prod <- e %*% matrix
    if(FALSE == identical(identity, prod))
    {
        print("Error: test failed due to unexpected result: 14")
        return(FALSE)
    }
    
    # Test with default value
    obj3 <- makeCacheMatrix()
    matrix1 <- obj3$get()
    matrix2 <- cacheSolve(obj3)
    if(nrow(matrix1) != nrow(matrix2) || ncol(matrix1) != ncol(matrix2))
    {
        print("Error: test failed due to unexpected result: 15")
        return(FALSE)
    }
    
    # Test with matrix #1. See if calculated value matches the
    # expected value.
    g <- rbind(c(2, 3, 1), c(2, 2, 2))
    obj4 <- makeCacheMatrix(g)
    matrix <- cacheSolve(obj4)
    
    TRUE
}
