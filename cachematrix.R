## Calculate the inverse of matrices of any size. 
## Cache the values to avoid recalculating every time
## Created: 2014.07.27
## Updated: 2014.07.27
## Author: Serban Tanasa


#First, clear the working environment
rm(list=ls())

## create a special matrix object with a few extra attibutes to store, 
## set, and access the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  #Start with a regular matrix;
  m <- NULL   #Create a null object m
  set <- function(y) {   ##create the function set() as an attribute of the object
    x <<- y              ##set our x, clear the m cach and store both with <<- Note:
    m <<- NULL           ##this is mostly useful if we want to later reuse the function on a new matrix.
  }
  get <- function() x    ##create the function get() as an attribute of the object
  setinverse <- function(solve) m <<- solve  #allow the inverse attribute to be set
  getinverse <- function() m     #allow the inverse attribute to be called up
  list(set = set, get = get,     ##return a list of this attributes as a function results
       setinverse = setinverse,
       getinverse = getinverse)  
  
  
}


## The solver function: checks if the inverse was already calculated
## IF so, returns the precalculated value (return(m))
## Otherwise, calculates it for the first time and stores it with setinverse()


#Note that the function must call the special "matrix" object we created previously. 
cacheSolve <- function(x, ...) {
  m <- x$getinverse() #Look to see if precalculated
  if(!is.null(m)) {     #if not null, tell user we're using the cache and return it
    message("getting cached data")
    return(m)           #return the cache and skip over the rest of the code
  }
  data <- x$get()       #otherwise, get the original matrix with the get() function we defined     
  m <- solve(data, ...)  #use the regular solve function to calculate the inverse
  x$setinverse(m)        #set the value of the cache m for all future calls
  m                      #return m;  
  }


## Demonstration area:
mat <- matrix(rnorm(1000000), ncol=1000)  ##Create a large enough matrix for the difference to be noticeable
system.time(testInv <- solve(mat)) #calculate it the old-fashioned way!
#  user  system elapsed 
#  0.59    0.00    0.60 
# It takes my Quadcore i7 processor 0.60 seconds to (singlethread) compute the inverse of a 1000x1000 matrix!

## create an instance of the object type we defined above.
mat2 <- makeCacheMatrix(mat)
##Look at the attributes
attributes(mat2)

#Time the cache function (first run)
system.time(cacheSolve(mat2))
#user  system elapsed 
#0.59    0.00    0.60 
#Same as the regular Solve, of course!

#Time the cache function (every other run)
system.time(cacheSolve(mat2))
#getting cached data
#user  system elapsed 
#0.03    0.00    0.02 

## 3000% speed gain! Wow. 

#Make sure we're getting the same results:
identical(cacheSolve(mat2), testInv)
##[1] TRUE  #Yay!

#OR call it straight as an attribute of mat2, once calculated
identical(mat2$getinverse(), testInv)
##[1] TRUE  #Yay!