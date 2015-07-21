## This is the assignement nr. 2 for the Programming in R course
## these 2 functions handle the matrix inversion by using a cache

## This function returns a list object that provides the functions
## to store the cached value

makeCacheMatrix <- function(x = matrix()) {

        mCache <- NULL    
        ## stores the matrix in the work environment
        set <- function(y) {
                x <<- y
                mCache <<- NULL
        }
        ## function to return the matrix
        get <- function() x
        ## function to invert the matrix and cache it
        setMatrix <- function(inverse) mCache <<- inverse
        ## function to return the inverted matrix
        getInverse <- function() mCache
        ## return list objectt
        list(
                set = set, 
                get = get,
                setMatrix = setMatrix,
                getInverse = getInverse)
               
}


## This function returns the inverse matrix fed to the previous function, 
## either inverting ## or getting it from cache, making sure it didn't change in the meanwhile

cacheSolve <- function(x, ...) {
        
        mCache <- x$getInverse()
        
        ## return inverted matrix from cache if it exists
        ## else goes on inverting the matrix
        if (!is.null(mCache)) {
                message("Returning from cache")
                return(mCache)
        }
        ## recover the original matrix
        matrix <- x$get()
        ## it the matrix is not square or the determinant=0
        ## return an error (I leave this to R error management)
        tryCatch( {
                mCache <- solve(matrix)
        },
        error = function(err) {
                message("Error:")
                message(err)
                return("")
        },
        warning = function(wrn) {
                message("Warning:")
                message(wrn)
                return("")
        },
        finally = {
                ##save the result in cache
                x$setMatrix(mCache)
        } )
        
        ##returns the matrix
        mCache
        
}

## Execution test
# > x = rbind(c(1,-2,3), c(4,-5,6))
# > x
# [,1] [,2] [,3]
# [1,]    1   -2    3
# [2,]    4   -5    6
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2] [,3]
# [1,]    1   -2    3
# [2,]    4   -5    6
# > cacheSolve(m)
# Error:
#         'a' (2 x 3) must be squareNULL
# #######################################
# 
# > x = rbind(c(1,-2,3), c(4,-5,6), c(7,-8,9))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2] [,3]
# [1,]    1   -2    3
# [2,]    4   -5    6
# [3,]    7   -8    9
# > cacheSolve(m)
# Error:
#         system is computationally singular: reciprocal condition number = 1.54198e-18NULL
# #########################################
# # > x = rbind(c(1, -1/4), c(-1/4, 1))
# # > m = makeCacheMatrix(x)
# # > m$get()
# # [,1]  [,2]
# # [1,]  1.00 -0.25
# # [2,] -0.25  1.00
# # > cacheSolve(m)
# # [,1]      [,2]
# # [1,] 1.0666667 0.2666667
# # [2,] 0.2666667 1.0666667
# # > cacheSolve(m)
# # Returning from cache
# # [,1]      [,2]
# # [1,] 1.0666667 0.2666667
# # [2,] 0.2666667 1.0666667

