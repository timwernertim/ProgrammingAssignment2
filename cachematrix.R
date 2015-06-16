## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function, makeCacheMatrix creates a 'matrix' containing a function to:
## construct a matrix (set),
## get the matrix (get),
## set the inverse of the matrix (set_inverse),
## and get the inverse of the matrix (get_inverse)

makeCacheMatrix <- function(input_matrix = matrix()) {
  inverse_cache <- NULL
  set <- function(y) {
    input_matrix <<- y
    inverse_cache <<- NULL
  }
  get <- function() input_matrix 
  set_inverse <- function(m) inverse_cache <<- m
  get_inverse <- function() inverse_cache
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse) 
}


## Write a short comment describing this function
## function returns cached inverse matrix if available, otherwise calcualtes the inverse of a matrix 
cacheSolve <- function(input_matrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_cache <- input_matrix$get_inverse()
  
  if(!is.null(inverse_cache)) { # check if input matrix is same as data
    message("getting cached data")
    return(inverse_cache)
  }
  data <- input_matrix$get()
  inverse <- solve(data, ...)
  input_matrix$set_inverse(inverse)
  inverse
}

# test matrix:  1 2 3
#              0 1 4
#              5 6 0

# Test run:
# seq1 <- c(1, 2, 3, 0, 1, 4, 5, 6, 0)
# mat1 <- matrix(seq1, 3)
# matrix<-makeCacheMatrix(mat1) ## create matrix
# cacheSolve(matrix)  ## calculate inverse for the first time
# output:         [,1] [,2] [,3]
#             [1,]  -24   20   -5
#             [2,]   18  -15    4
#             [3,]    5   -4    1
# cacheSolve(matrix) # fetch cached inverse matrix 
# output: getting cached data
#             [,1] [,2] [,3]
#       [1,]  -24   20   -5
#       [2,]   18  -15    4
#       [3,]    5   -4    1

# change matrix and check if it will be recalculated
#test matrix 2: 4 3
#               3 2
#> seq2<-c(4,3,3,2)  
#> mat2 <- matrix(seq2, 2) ## create matrix
#> matrix2<-makeCacheMatrix(mat2)
#> cacheSolve(matrix2)
#[,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
#> cacheSolve(matrix2)
#[,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
#getting cached data
#[,1] [,2]
#[1,]   -2    3
#[2,]    3   -4

