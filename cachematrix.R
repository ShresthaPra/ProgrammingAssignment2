## These functions show how to use lexical scoping - it allows to use some of the fields(in this case vector) to cache static values


## The first function(makeCacheMatrix) creates a special vector
## (setMatrixA -> to set a matrix value; getMatrixA -> to get a matrix value from what was already set; 
## setMatrixAInverse -> to set a matrix Inverse; getMatrixAInverse -> to get the matrix inverse already set)

makeCacheMatrix <- function(x = matrix()) {
    ##Seting MatrixA as NULL
    matrixA <-NULL
    
    ##setMatrixA-> to set up the matrix(lets say A) which is to be Inversed and nullify the cached matrixA(Inversed Matrix)
    setMatrixA<-function(y){
        x<<-y
        matrixA<<-NULL
    }
    ##getMatrixA-> to get back(inputted A) the matrix that was set up
    getMatrixA<-function(){x}
    
    ##setMatrixAInverse-> to set the A Inverse(cacluclated in cacheSolve) to MatrixA
    setMatrixAInverse<-function(matrixAInverse){matrixA<<-matrixAInverse}
    ##getMatrixAInverse-> to get the MatrixA(Inverse of A) value which was already set in setMatrixAInverse
    getMatrixAInverse<-function(){matrixA}
    ##creates a special vector
    list(setMatrixA=setMatrixA,getMatrixA=getMatrixA,setMatrixAInverse=setMatrixAInverse,getMatrixAInverse=getMatrixAInverse)
}


## Return a matrix that is the inverse of 'x' if cached then the previous value otherwise a new caclulated value 

cacheSolve <- function(x, ...) {
    ## Createed variable to store Inverse by using the special vector property of getting Matrix a Inverse
    matrixAInverse<-x$getMatrixAInverse()
    ##Checking if the value is already present 
    if(!is.null(matrixAInverse))
    {
        ##Giving out a message if Matrix is already calculated or not
        message("Getting Matrix A Inverse value through Cache")
        ##Returning the previously cached value
        return(matrixAInverse)
    }
    ##getting value of x
    matrixA <- x$getMatrixA()
    
    ##Solving matrix A Inverse
    matrixAInverse<-solve(matrixA)
    
    ##setting value of Inverse to matrixAInverse
    x$setMatrixAInverse(matrixAInverse)
    
    ##Printing matrixAInverse
    matrixAInverse
}
