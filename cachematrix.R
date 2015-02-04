###This function caches the inverse of a matrix                                  
## This function actually creates a list containing 4 functions to cache the inverse, 
## which is convenient for following manipulation

makeCacheMatrix <- function ( x = matrix()){   
    m <- NULL
    set <- function (y){  ##set the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x  ##get the value of the matrix
    setinverse <- function(inverse) m <<- inverse  ##set the value of the inverse
    getinverse <- function() m  ##get the value of the inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function calsulates the inverse of the matrix subject
## created by makeCacheMatrix()
cacheSolve <- function(x, ...){
    m <- x$getinverse()
    if(!is.null(m)){  ## to see if the inverse has been calculated
        message("getting cached data")
        return(m)
    }
    data <- x$get()  ##calculate the inverse of the matrix
    m <- solve(data,...)
    x$setinverse(m)
    m
}
