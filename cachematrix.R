## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
  #initialize inverse as a NULL
  invs <- NULL
  #set function will assign new
  set <- function(y)
  {
    #parent environment
    x <<- y
    #for new matrix, reset inverse to NULL
    invs <- NULL
  }
  
  get <- function() x
  #inverse assigned to parent environment
  setInvs <- function(inverse) invs <<- inverse
  #finds where it's called and gets it
  getInvs <- function() invs
  list(set = set, get = get, setInvs = setInvs, getInvs = getInvs)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  invs <-x$getInvs()
  if(!is.null(invs))
  {
    message("getting cached data")
    return(invs)
  }
  
  data <- x$get()
  invs <- solve(data, ...)
  x$setInvs(invs)
  invs       
}
