## Put comments here that give an overall description of what your functions do


## Write a short comment describing this function
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## 1. Checks to see if the inverse has already been calculated. If so, it gets the inversie from the cache
## 2. If not, the inverse of the data will be calculated and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


## Testing MakeCachematrix

##> x = rbind(c(1, 4), c(-1, 4))
##> m = makeCacheMatrix(x)
##> m$get()
##      [,1] [,2]
## [1,]    1    4
## [2,]   -1    4

## Testing cacheSolve

## No cache yet:
##> cacheSolve(m)
##      [,1]   [,2]
## [1,] 0.500 -0.500
## [2,] 0.125  0.125

## Cache stored above, so now we do have a cache:
## > cacheSolve(m)
## getting cached data
##      [,1]   [,2]
## [1,] 0.500 -0.500
## [2,] 0.125  0.125
