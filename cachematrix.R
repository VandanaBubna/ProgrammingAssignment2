## There are two functions "functionmakeCacheMatrix" and functioncacheSolve

## The first function takes a matrix as an argument and creates a special vector to get and set the values of a vector and the inverse
function

makeCacheMatrix <- function(x = matrix()) 
{

	i <- NULL
        
	set <- function(y) 
	{
                
		x <<- y
                
		i <<- NULL
        
	}
        
	get <- function() x
        
	setinverse <- function(inverse) i <<- inverse        
	getinverse <- function() i       
	list(set = set, get = get,
 setinverse = setinverse,
getinverse = getinverse)
}



## This function calculates the inverse of the special vector and if already calculated before ,skips the computation and takes the value from cache
function

cacheSolve <- function(x,...) 
{
 
    i <- x$getinverse()
        
	if(!is.null(i)) 
	{
                
		message("getting cached data")
                
		return(i)
        
	}
        
	data <- x$get()
        
	i <- solve(data)
        
	x$setinverse(i)
        
	i   ## Return a matrix that is the inverse of 'x'

}
