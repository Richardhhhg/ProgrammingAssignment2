# creating a matrix whose inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
    # assigning nulls to matrix that has not been cached
    inverse_matrix <- NULL
    
    # creating the inverse matrix
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    
    # representation of the vector
    get <- function() x
    
    # storing the inverse matrix
    set_inverse_matrix <- function(inverse) inverse_matrix <<- inverse 
    
    # returning the inverse matrix
    get_inverse_matrix <- function() inverse_matrix
    list (set = set, get = get,
          set_inverse_matrix = set_inverse_matrix, 
          get_inverse_matrix = get_inverse_matrix)
}

# returning the inverse matrix 
cacheSolve <- function(x, ...) {
    # assigning the current matrix to the inverse matrix variable
    inverse_matrix <- x$get_inverse_matrix()
    
    # if the inverse matrix has been cached, the inverse matrix is returned
    if(!is.null(inverse_matrix)) {
        return(inverse_matrix)
    }
    
    # if the inverse matrix has not been cached, it gets solved and cached
    data <- x$get()
    inverse_matrix <- solve(data)
    x$set_inverse_matrix(inverse_matrix)
    inverse_matrix
}
