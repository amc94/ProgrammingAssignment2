## makes a matrix cache object that caches its own inverse

makeCacheMatrix <- function(x = matrix()) {
        mat_inverse <- NULL
        
        set <- function(y) {
                x <- y
                mat_inverse <- NULL
        }
        
        get <- function() x
        
        set_inverse <- function(inv){
                mat_inverse <<- inv
        }
        
        get_inverse <- function() mat_inverse
        
        list(set=set, get=get,
             set_inverse=set_inverse,
             get_inverse=get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inverse <- x$get_inverse()
        
        if(!is.null(mat_inverse)){
                message("getting cached data")
                return(mat_inverse)
        }
        
        mat <- x$get()
        mat_inverse <- solve(x$get(), ...)
        x$set_inverse(mat_inverse)
        mat_inverse
}

##Tests
mat <- matrix(c(1,2,3,4),2,2)
mat_inverse <- solve(mat)

cached_mat <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
stopifnot(identical(mat, cached_mat$get()))

cacheSolve(cached_mat)
stopifnot(identical(mat_inverse,cached_mat$get_inverse()))

cacheSolve(cached_mat)
