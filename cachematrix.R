## The below pair of functions cache the inverse of an invertible matrix. 

## The first function, makeCacheMatrix creates a special matrix, which is really 
## a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the matrix's inverse
# get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        rc <- sqrt(nrow(as.data.frame(x)))
        x <- matrix(x,nrow=rc,ncol=rc)
        xi <- NULL
        set <- function(y) {
                x <<- y
                xi <<- NULL
        }
        get <- function() x
        setinv <- function(inv) xi <<- inv
        getinv <- function() xi
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the matrix created with the above 
## makeCacheMatrix function. However, it first checks to see if the inverse has already 
## been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value in the cache via  
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xi <- x$getinv()
        if(!is.null(xi)) {
                message("getting cached data")
                return(xi)
        }
        xi <- solve(x$get())
        x$setinv(xi)
        xi
}

# x <- makeCacheMatrix()  # makes and sets the matrix from the function call. Avoids need for
# x$set()
x <- makeCacheMatrix(c(2,3,4,5,2,3,2,1,7)) 
x <- makeCacheMatrix(c(4,0,0,4))
x <- makeCacheMatrix(c(4,2,1,4))
x <- makeCacheMatrix(c(2,0,0,0,2,0,0,0,2))
#x$set()
x$get()
# The value of x$setinv is determined by the function and cannot be manually specified.
x$getinv()
cacheSolve(x)
x$getinv()
cacheSolve(x)
