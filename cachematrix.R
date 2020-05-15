## functions that cache the inverse of a matrix for optimising calculations

## matrix object that caches it's inverse
## makes a list of 4 functions as specified
makeCacheMatrix <- function( m = matrix() ) 
{        
        ## initialised inverse to NULL
        i <- NULL
        
        ## set function to set the matrix to m
        set <- function( matrix ) 
        {
                m <<- matrix
                i <<- NULL
        }

        ## get function to return the set matrix
        get <- function() 
        {
                m
        }

        ## set the inverse of the matrix to i
        setInverse <- function(inverse) 
        {
                i <<- inverse
        }

        ## function to return the inverse of the matrix
        getInverse <- function() 
        {
                i
        }

        ##return the list of 4 functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## computes the inverse of matrix if it is not cached else returns the cached value
cacheSolve <- function(x, ...) 
{
        ## get inverse of x and store in m
        m <- x$getInverse()

        ## check if it is cached
        if( !is.null(m) ) 
        {
                message("getting cached data")
                return(m)
        }

        ## else calculate inverse and store it in cache
        y <- x$get()
        m <- solve(y) %*% y
        x$setInverse(m)

        ## return inverse
        m
}
