## Create the Matrix. check if it is a square
makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL

        ##Sanity check: make sure the matrix is a square and thus inversible
        num_rows <- nrow(x)
        num_cols <- ncol(x)
        if(num_rows != num_cols){
            warning("Matrix passed is not a square!!\n");
        }

        ##Initialize matrix. Set inverse to NULL (indicating it has not been 
	##computed)
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        
        get <- function() x

        setinverse <- function(y_inverse) x_inverse <<- y_inverse

        getinverse <- function() x_inverse

        ##Initialize matrix and return list of methods
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##Compute Matrix Inverse
cacheSolve <- function(x, ...) {

        ##First see if inverse is already available in Cache
        x_inverse <- x$getinverse()
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(x_inverse)
        }

        ##This is the branch if inverse is not yet cached
        data <- x$get()
        x_inverse <- solve(data, ...) ## Compute inverse
        x$setinverse(x_inverse)       ## Cache inverse for the future calls
        x_inverse                     ## Return inverse
}
