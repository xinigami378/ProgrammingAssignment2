## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( mat = matrix() ) {
    
    ## Initialize the inverse property
    ini <- NULL
    
    ## Method to set the matrix
    set <- function( matrix ) {
        mat <<- matrix
        ini <<- NULL
    }
    
    ## Method the get the matrix
    get <- function() {
        ## Return the matrix
        mat
    }
    
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        ini <<- inverse
    }
    
    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        ini
    }
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getInverse()
    
    ## Just return the inverse if its already set
    if( !is.null(mat) ) {
        message("getting cached data")
        return(mat)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    mat <- solve(data) %*% data
    
    ## Set the inverse to the object
    x$setInverse(mat)
    
    ## Return the matrix
    mat
}