## makeCacheMatrix will create an object to store two matrices along with function to manipulate these.
## cacheSolve will be able to use an object create by makeCacheMatrix to calculate the inverse matrix, and
## then store it in the original object.

## Creates an object to hold two vectors with getters and setters.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL 
    
    set <- function(y){
        x <<- y # Set internal matrix object
        im <<- NULL # Set internal inverse matrix object to null
    }
    get <- function(){
        x # Return the internal matrix object
    }
    getInverse <- function(){
        im # Return the cached inverse matrix (or NULL if not calculated) 
    }
    setInverse <- function(inv = matrix()){ # Check for a matrix variable
        im <<- inv # Set the internal matrix object to the matrix var.
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve will take an object create by makeCacheMatrix and either retrieve or calculate and
## set the objects internal inverse matrix.

cacheSolve <- function(x, ...) {
        im <- x$getInverse() # Get the Inverse matrix, or NULL if not set
        
        if (!is.null(im)){ # Check for non-null value
            message ("Getting cached data") # Play nice and send a diagnostic message
            return (im) # Return the allready calculated inverse, use return to exit the function
        }
        data <- x$get() # Inverse was not calculated, get the matrix
        im <- solve(data, ...)  # Use solve to calculate the Inverse matrix, and also use
                                # triple dots to pass on any extra vars from function call
        x$setInverse(im) # Set the cahce object to the fresly calculated
        im # And also return it 
}
