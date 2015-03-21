## Inverse of a matrix is calculated once and the result is cached and retrieved for further calls on the same matrix.


## creates a matrix(list) which can cache the results of a inverse operation for further retrievals. Both orignal matrix and its inverse are stored
makeCacheMatrix <- function(x = matrix()) 
{
  
  #xinv is the inverse matrix
  xinv <- NULL
  
  # sets the actual matrix, x
  set <- function(y) 
  {
  	# the matrix x is set, as x is in a diffent environment from y, <<- is used to set it.
    x <<- y
    
   #  <<- as xinv is in another environment than the current function. 
   # Also useful when a matrix is changed, the inverse is set to null so that a new inverse is computed at next invocation

    xinv <<- NULL
  }
  
  # returns the actual matrix x. the search is first done with in the get function environ and then the parent of this environ where x is found
  get <- function() x 
  
  
  # the setinverse function gets the inverse matrix, invmat, and sets it to the var xinv. Since Xinv is outside the environment of setinverse function it uses the <<- operator
  setinverse <- function(invmat) {
    xinv <<- invmat                           
  }
  
  # returns the inverse matrix.
  getinverse <- function(){ xinv }
  
  # create the list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## uses solve method to find the inverse of a matrix for the first time. And retrieves a cached result if the matrix remains the same for next invocations

cacheSolve <- function(x, ...) 
{
	
  # Get the cached inverse result
  inverse <- x$getinverse()
  
  # see if the inverse is not null, if yes, it means inverse has been calculated for this matrix so return the cached result
  if(!is.null(inverse)) 
    {
    message("getting cached data")
    return(inverse)
  }
  
  # if the flow reaches here, it means inverse is null and the inverse has not been calculated yet.
  
  # now get the original matrix
  data <- x$get()
  
  # calculate the inverse of this matrix
  inverse <- solve(data)
  
  # store/cache the result from above for next retrievals
  x$setinverse(inverse)
  
  # return the inverse
  inverse   
}
