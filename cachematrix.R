## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This matrix create a cache-able matrix object
# That can hold both a matrix and it's inverse
# This solution builds upon the example fmakor the
# cache mean calculations, specificaly, makeVector

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(inputm){
    m <<- inputm
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) inv <<- inverse # main addition
  getinv <- function() inv 
  list(set = set, get = get,
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function

# This function returns the inverse of the cache-able
# matrix object m, either from the previously cached
# result, or calculating in the function call. 
# In the later case, it stores the inverse in the 
# inv component of the matrix object m.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- m$getinv()
  if(!is.null(inv)){     # If the result was previously cached
    message("getting cached data")
    return(inv)          # just return
  }
  data <- m$get()        # else
  inv <- solve(data,...) # calculate
  m$setinv(inv)          # cache the result
  inv                    # and return
}
