## the fonction takes a matrix and keps its matrix in cache, if called repeatedly, 
## it tells that it's getting the data from cache without repeatative calculation

## This function makes the inverse and stores it in the cache.

makecacheMatrix <- function(x= matrix()) {
  inmat <- NULL
  set <- function(y) {
    x <<- y
    inmat <<- NULL
  }
  get <- function() x
  setmat <- function(solve) inmat <<- solve
  getmat <- function() inmat
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}

##########################################################
# this function compares the exsiting condition andecides wheather it need to calculate again 
#or it can pass the matrix stored.

cachesolve <- function(x, ...) {
  inmat <- x$getmat()
  if(!is.null(inmat)) {
    message("getting cached data")
    return(inmat)
  }
  data <- x$get()
  inmat <- solve(data, ...)
  x$setmat(inmat)
  inmat
}

##########################################################

# example
a<- matrix(rnorm(16, 5,4), nrow=4)
a
b<- makecacheMatrix(a)
b
inv <-cachesolve(b)
l <- a %*% inv
l
