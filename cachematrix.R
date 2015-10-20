## makeCacheMatrix and cacheSolve are two function to cache potentially time consuming computing 


## makeCacheMatrix function has four functions - set(), get(), setinverse(), getinverse()

makeCacheMatrix <- function(x = matrix()) {
	set <- function(x,r,c){
              		y <<- matrix(x,nrow = r,ncol = c)
              		inv <<- NULL
	      }	
        	
        	get <- function()y
        	setinverse <- function(value) inv <<- value
        	getinverse <- function() inv
        	list(set=set,get=get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function calculates inverse. but it first checks if inverse is already been 
##calulated then it directly gives away value from cache
##saving computational time or calculates the va;ue
##and gives inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
          	if(!is.null(inv)) {
                 		message("getting cached data")
                 		return(inv)
    	       } 
            	data <- x$get()
            	inv <- solve(data, ...)
            	x$setinverse(inv)
            	inv
}
