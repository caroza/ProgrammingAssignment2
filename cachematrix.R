## Function makeCacheMatrix creates and returns a list of functions
## which allow a calling function to set a matrix in the cache, retrieve it,
## set the inverse, or retrieve the inverse.
## This will allow a previously computed inverse to be retrieved from cache without having to redo
## the computation.
## The function accepts one argument, a matrix which is assumed to be both square (nrows = ncolumns)
## and invertible (determinant <> 0)
## The component functions are:
## set: a function which loads a matrix into the cache
## get: a function which retrieves the matrix from cache.  This function allows a calling function
## to check whether the matrix (and its inverse) are already cached
## setInverse: a function which loads the inverse of the matrix into the cache
## getInverse: a function which retrieves the previously loaded inverse from the cache
makeCacheMatrix <- function(x = matrix()) {
        # initialize the cache to ensure that the results of previous 
        # executions are cleaned out.  This is because the cache variables are
        # defined in the parent of the environment where the function
        # is defined.
        invcache <- NULL

        # load an inputted matrix into cache
        set <- function(y) {
                x <<- y
                invcache <<- NULL # if a new matrix is set, clean out the inverse
                									# so previously computed inverses are not associated
                									# with the wrong matrix
        }

        # retrieve the matrix from cache
        get <- function() {
        return(x)									# get previously loaded matrix from cache
        }
        # invert the matrix and load into cache
        setInverse <- function(inverse) {
        							invcache <<- inverse
        }
        # get the inverted matrix from cache
        getInverse <- function() {
        							return(invcache)
        }
				# return a list which allows the component functions to be accessed 
        # from the working environment
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function cacheSolve calculates the inverse of a matrix
## If the matrix and its inverse do not exist in the cache,
## cacheSolve will compute the inverse using Solve and then
## load the matrix and its inverse into cache using the 
## component functions defined in makeCacheMatrix.
## If the matrix and its inverse already exist in the cache,
## the inverse is retrieved from cache rather than being
## computed on every call.
## The function accepts as its first argument a matrix which is assumed to be both square (nrows = ncolumns)
## and invertible (determinant <> 0)
cacheSolve <- function(x = matrix(), ...) {
        ## Are the matrix and its inverse in the cache already?
        M <- x$getInverse()

        # return inverted matrix from cache if it exists
        # otherwise load the matrix into the cache
        if (!is.null(M)) {
                message("getting cached data")

                # return matrix inverse to console
                return(M)
        }

        # Matrix is not cached to solve it and store the inverse
        # create matrix since it does not exist
        # Note assume matrix is square and invertible for this exercise
        NewM <- x$get()
				# set and return inverse of matrix
        M <- solve(NewM, ...)
        # set inverted matrix in cache
        x$setInverse(M)
        # display matrix in console
        return (M)
}
