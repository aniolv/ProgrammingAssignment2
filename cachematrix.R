makeCacheMatrix <- function(x = matrix()) {
        returned <- NULL
		#returned value will be the variable that will contain the returning object
        setmatrix <- function(assigned) {
                x <<- assigned
                returned <<- NULL
        }
        getmatrix <- function() x
        setsolved <- function(inverse) returned <<- inverse
        getsolved<- function() returned
        listm<-list(setmatrix = setmatrix, getmatrix = getmatrix,
             setsolved = setsolved,
             getsolved = getsolved)
		#listm as a list with all the functions to use in this makeCachematrix.This functions
		#will set the values of the matrix and get them, and the same with the solved matrix.
}

cacheSolve <- function(x, ...) {
        #we look at the cache to see if the amtrix is already there. This will be that the position in the list is not empty
        returned <- x$getsolved()
        if(!is.null(returned)) {
                message("getting cached data")
                return(returned)
        }
		#if the value is not in the cache function, return will become the slved version of the matrix that is new
        datam <- x$getmatrix()
        returned<-solve(datam)
        x$setsolved(returned)
        returned
}
