## Functions creates the object of the matrix. 
## Its allows to set/change/get the matrix. Calculate and cach the inverse matrix of the original mattrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix = matrix()
      
      #This function sets the values of matrix elements
      setMatrix <- function(newMatrix){
            M <<- newMatrix
            inverseMatrix = matrix()
      }
      
      #This f. returns the matrix
      getMatrix <- function() M
      
      #This f. sets the inverse matrix of the original one, by calling external f. onverseMatrix
      setInverse <- function(inverse) inverseMatrix <<- inverse
      
      #This f. returns the inverse matrix of the original matrix
      getInverse <- function() inverseMatrix
      
      list( setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
   
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inverse <- M$getInverse() ##gets cached inversed matrix
      mat <- M$getMatrix()      ##gets original matrix
      
      ##checks if the matrix was changed and if cached inverse matrix exists
      if ((!is.na(inverse))&&(identical(M==mat)=TRUE)){         
            message("getting cached inverse matrix")
            return(inverse)
      }
      
      ##calculate inverse matrix
      inverse <- solve(mat)
      ##sets inverse matrix as a parametr of the original object
      M$setInverse(inverse)
      inverse
}
