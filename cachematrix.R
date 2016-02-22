## Overall discription:
# Calculate repeatively could be computational expensive.
# In this excerise, we generate cache to store the inverse 
# matrix of 'x', to avoid repeat calculation.

## Function description:
# create a cache to store inverse matrix of 'x'
makeCacheMatrix <- function(x = matrix()) {
  # set default inverse matrix
  inverse.matrix <- NULL
  # set the matrix
  set <- function(y) {
    x <<- y
    inverse.matrix <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the inverse matrix
  set.inverse.matrix <- function(inverse) inverse.matrix <<- inverse
  # get the inverse matrix
  get.inverse.matrix <- function() inverse.matrix
  # generate a list that can be used as input for cacheSolve()
  list(set = set, get = get,
       set.inverse.matrix = set.inverse.matrix,
       get.inverse.matrix = get.inverse.matrix)
}

## Function description:
# Return a matrix that is the inverse of 'x', but before
# genereate a inverse matrix, check the cache to see if 
# inverse matrix has already been calculated and stored.
# If there is no previously calculated inverse matrix, 
# calculate the inverse of 'x' and store in cache.
cacheSolve <- function(x, ...) {
  # get the inverse matrix for the input list under get.inverse.matrix
  inverse.matrix <- x$get.inverse.matrix()
  # if the inverse matrix already be calculated and stored in inverse.matrix
  if(!is.null(inverse.matrix)) {
    message("getting cached inverse matrix")
    # get the stored inverse matrix
    return(inverse.matrix)
  }
  
  # if inverse matrix has not been calculated, get the data (matrix.data)
  # and calculated by solve()
  matrix.data <- x$get()
  inverse.matrix <- solve(matrix.data, ...)
  x$set.inverse.matrix(inverse.matrix)
  return(inverse.matrix)
}