## The First Fuction makeCacheMatrix creates a special matrix, 
 # Which is containing following functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value for the inverse of a matrix
## 4. get the value for the inverse of a matrix   

makeCacheMatrix <- function(mat = matrix()) {
        # Initialy set inverse value as null.
        inv<-NULL
        setMatrix<-function(y){
                mat<<-y
                inv<<-NULL
        }
        getMatrix<-function() mat
        setInverseMatrix<-function(InverseMatrix) inv<<- InverseMatrix
        getInverseMatrix<-function() inv
        list(setMatrix=setMatrix, getMatrix=getMatrix,
             setInverseMatrix=setInverseMatrix,
             getInverseMatrix=getInverseMatrix)
}


## The Second Fuction cacheSolve  computes the inverse of the special "matrix" 
 # returned by makeCacheMatrix above.

## If the inverse has already been calculated (and the matrix has not changed),
 # then the cachesolve should retrieve the inverse from the cache.     

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-mat$getInverseMatrix()
        if(!is.null(inv)){
                message("getting cached inverse matrix")
                return(inv)
        }
        Matrix<-mat$getMatrix()
        inv<-solve(Matrix, ...)
        mat$setInverseMatrix(inv)
        inv
}
