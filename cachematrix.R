## These 2 functions will take a matrix and put it into a cache, then 

makeCacheMatrix <- function(x = matrix()) {    
    m <- NULL               ## initialize m
    set <- function(y) {    ## define the 'set' function, with argument y
        x <<- y             ## set y equal to x, WITHIN the set function
        m <<- NULL          ## initialize m WITHIN the set function
    }
    get <- function() x     ## define the 'get' function, with argument x
    setinv <- function(solve) m <<- solve  ## define 'setinv' function, calling solve function on m
    getinv <- function() m  ## define 'getinv' function, which calls m as an argument
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)     ## create a list with the arguments
}


## Checks whether result is cached, if not calculates inverse and returns it

cacheSolve <- function(x, ...) {
    m <- x$getinv()                 ## store x$getinv() (aka matrix inverse) in m
    if(!is.null(m)) {               ## check if m already exists
        message("getting cached data")  ## if m already exists, return message
        return(m)                       ## if m already exists, print m
    }
    data <- x$get()             ## assign 'get' element of list to data
    m <- solve(data, ...)       ## assign results of 'solve' function (aka the inverted matrix) to m
    x$setinv(m)                 ## retrieve x's inverted matrix
    m                           ## print the inverted matrix m
}
