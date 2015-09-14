## These functions allow the construction of a special matrix that can cache its inverse.  
##
## Example usage :
##
##  cm <- makeCacheMatrix( some_matrix )    # Construct a matrix that can cache its inverse
##
##  inverse <- cacheSolve( cm )             # The first call will calculate the inverse
##
##  inverse <- cacheSolve( cm )             # Subsequent calls will return the cached inverse


## Construct a special matrix that allows the caching of the matrix inverse

makeCacheMatrix <- function( x = matrix() ) {
    
    # the cached matrix inverse
    inv <- NULL
    
    # set the matrix data and clear the cache
    set <- function( y ) {
        x <<- y
        inv <<- NULL
    }
    
    # get the underlying matrix data
    get <- function() x
    
    # caches the matrix inverse
    setinverse <- function( inverse ) {
        inv <<- inverse
    }
    
    # gets the cached inverse
    getinverse <- function() {
        inv
    }
    
    # return the special matrix
    list( set=set, get=get, setinverse=setinverse, getinverse=getinverse )
}


## Calculates the inverse of the given matrix and caches its inverse

cacheSolve <- function( x, ... ) {
    
    ## If we have a cached inverse, return it
    
    inv <- x$getinverse()
    
    if ( ! is.null( inv ) ) {
        message( "getting cached data" )
        return( inv )
    }
    
    ## No cached inverse : calculate it, cache it and return it
    
    data <- x$get()
    
    inv <- solve( data, ... )
    
    x$setinverse( inv )
    
    inv
}
