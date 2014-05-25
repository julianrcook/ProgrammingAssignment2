# There are two functions: makeCacheMatrix, that creates a cacheable version of matrix 'x'
# and also cacheSolve, which inverts the matrix and caches the inverse for subequent calls

# An example of use would be
# matX <- matrix(1:4, 2, 2)
# SpecialmatX = makeCacheMatrix(matX)
# InvMatX_1stCall = cacheSolve(SpecialmatX)
# InvMatX_2ndCall = cacheSolve(SpecialmatX)
# The 2ndcall should print .. 'getting cached inverse'

# The first function, makeCacheMatrix creates a special "matrix" object, which is really a list-type object
# containing methods that
# 1. set the value of the matrix and # 2. get the value of the matrix
# 3. set the value of the inverse and # 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix creates a special "matrix" list object like this:
  # list(set = set, get = get, setinv = setinv, getinv = getinv)
  inv <- NULL
  # set() will set the value of the matrix
  # get() will return the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
# The following function calculates the inverse of the special "matrix"
# created with makeCacheMatrix(). However, it first checks to see if
# the inverse has already been calculated. If so, it gets the inverse from
# the cache and skips the computation. Otherwise, it calculates the inverse
# of the matrix and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # If inv contains a value, it will be the previously cached value of the inverse
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}