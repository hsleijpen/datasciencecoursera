## The functions hereinafter are designed to create a matrix as well as to calculate the 
## calculate the inverse of that matrix.
## This in order to execute a repeating calculation of the inverse of this matrix more efficiently.

## The function makeCacheMatrix is to get and set a matrix, get and set the contents of that matrix
## and cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## The function cacheSolve calculates the inverse of the matrix created with the function above.
## Before doing this, however, it checks whether the inverse has already been calculated. 
## If so, the inverse is retrieved from the cache and the computation is skipped.
## Otherwise, it calculates the inverse of the matrix and stores this in the cache via the 
## setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("retrieving cached matrix")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
