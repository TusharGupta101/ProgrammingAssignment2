x <- matrix(c(1, 2, 3, 4), 2, 2)
m <- makeCacheMatrix(x)
cacheSolve(m)x <- matrix(c(1, 2, 3, 4), 2, 2)makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() {
        x
    }

    setinverse <- function(inverse) {
        m <<- inverse
    }

    getinverse <- function() {
        m
    }

    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

cacheSolve <- function(x, ...) {
    m <- x$getinverse()

    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    data <- x$get()
    m <- solve(data)
    x$setinverse(m)

    m
}