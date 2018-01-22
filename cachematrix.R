###################################################################################
####### Simples functions that cache the inverse of a matrix in order to  #########
####### avoid repeat calculations of very large matrices                  #########
###################################################################################


makeCacheMatrix <- function(A = matrix()) {
    ## Functions used to chache the inverse of a matrix, in order
    ## to avoid repeat calculations for large matrices (even though you really
    ## cannot assume that all matrices can be solved with the solved routine, that 
    ## is just silly). Creates a special matrix object where the inverse can be 
    ## cached/stored after the solve routine has been used once
    
    mat <- NULL
    set <- function(y){
        A <<- y
        mat <<- NULL
    }
    get <- function() A
    setinv <- function(solve) mat <<- solve
    getinv <- function() mat
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv) 
}


cacheSolve <- function(A, ...) {
    ## Gives the inverse of the special matrix A in the function makeCacheMatrix using the solve() routine 
    ## in R. If the inverse has already been calculated, this function simply retrieves the calculated 
    ## inverse of the matrix
    
    
    mat <- A$getinv()
    if(!is.null(mat)){
        message("Getting chached matrix")
        return(mat)
    }
    data <- A$get
    mat <- solve(data, ...)
    A$setinv(mat)
    mat
}



