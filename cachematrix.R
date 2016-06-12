## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## The pair of functions below cache the inverse of a matrix.

## This function creates a special "matrix", which is really a list containing a
## function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.
## Assumption: the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Output sample
## > sample_matrix <- makeCacheMatrix(matrix(c(4,7,2,6),2,2))
## > sample_matrix$get()
## [,1] [,2]
## [1,]    4    2
## [2,]    7    6
## > cacheSolve(sample_matrix)
## [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4
## > cacheSolve(sample_matrix)
## getting cached data.
## [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4