## These functions are meant to be used together.
##
## makeCacheMatrix sets and caches the value of the matrix. 
##
## cacheSolve runs a matrix inversion calculation and caches the answer, 
## but first checks to see if the solution has already been cached.
##
## Syntax and steps to call the functions:
##   1. > source('cachematrix.R')
##   2. > v$set(matrix(1:4,2,2))
##   3. > cacheSolve(v)

## Create the function to store the data in a cached variable.
makeCacheMatrix <- function(x = matrix()) {
  ## Clear the myCache var so previous runs of the function don't interfere.
  myCache <- NULL
  ## Store the value of the set request in x, cached. Then clear myCache's
  ## cached value, so we can populate it again later.
  set <- function(y) {
    x <<- y
    myCache <<- NULL
  }
  ## Return the value of x as set above in the set function
  get <- function() x
  ## Pass the value of the arg (inverse) into myCache, and cache myCache
  setInv <- function(inverse) myCache <<- inverse
  ## Return myCache when called
  getInv <- function() myCache
  ## Create a list with the values we created above
  list(set=set,
        get=get,
        setInv=setInv,
        getInv=getInv)
}

## Create the function to run the matrix inversion calculation
cacheSolve <- function(x, ...) {
  ## Get the value that we set and stored in makeCacheMatrix()'s myCache var
  myCache <- x$getInv()
  ## If myCache is not empty, tell the user that we're retrieving the 
  ## cached myCache var
  if(!is.null(myCache)) {
    message("Getting cached data...")
    ## Return the solve() calculation value
    return(myCache)
  }
  data <- x$get()
  ## Here we run the matrix inversion calculation
  myCache <- solve(data, ...)
  ## Populate the myCache var with the solve() calculated data.
  x$setInv(myCache)
  ## Return myCache, which now has the value of solve()
  myCache
}