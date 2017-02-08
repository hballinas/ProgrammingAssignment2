# Just making sure I don't change name of file
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # i is the inverse matrix of m and is initialiazed as null
    set <- function(y) {# the set function may be used to change the matrix without re-initializing makeCacheMatrix
        x <<- y # this assigns the value of y to m in the parent environment
        m <<- NULL # this assigns null to m in the parent environment
    }
    get <- function() x  # this gets m from the parent environment
    setinverse <- function(inverse) m <<- inverse # this assigns the value inverse to i in the parent environment
    getinverse <- function() m # this gets the inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) # this returns a list of named functions
}


cacheSolve <- function(x, ...) {
    m <- x$getinverse() # it gets the inverse matrix from makeCacheMatrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    } # this checks if i (the inverse matrix) is null or not. If not, it 
    +        # it return the cached i
    data <- x$get() # if i is null, then it gets m from makeCacheMatrix
    m <- solve(data, ...) # it calculates the inverse of matrix and assigns it to i
    x$setinverse(m) # in the setinverse function within makeCacheMatrix the inverse matrix 
    +    # is assigned to i in the parent environment
    m
}