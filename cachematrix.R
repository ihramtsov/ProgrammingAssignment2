##calculate inverse matrix with cache


##function-constructor for matrix cache
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    list(
            setMatrix = function(newMatrix){
                x <<- newMatrix
                inverseMatrix <<- NULL
            },
            getMatrix = function() x,
            getInverseMatrix = function() inverseMatrix,
            setInverseMatrix = function(inverseMatrixValue){
                inverseMatrix <<- inverseMatrixValue
            }
        )
}

##calculate inverse matrix, get inverted matrix from the cache if any, overwise calculate it and cache
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverseMatrix()
    if (!is.null(inverseMatrix)){
        message("get inverse matrix from cache")
        return(inverseMatrix)
    }
    
    matrix <- x$getMatrix()
    inverseMatrix <- calcInverseMatrix(matrix, ...)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix    
}

##calculate inverse matrix with validation: only square non-singular two-dimentional matrix is acceptable
##terminate an execution if the validation failed.
calcInverseMatrix <- function(m, ...){
    if (!is.matrix(m) || (dim(m) != 2) || (nrow(m) != ncol(m)))
        stop("provide square two-dimentional matrix, please")
    
    if (det(m) == 0)
        stop("singular matrix was provided")
        
    solve(m)
}
