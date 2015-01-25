## Put comments here that give an overall description of what your
## functions do

## This function takes in an invertible matrix
## It creates a special "matrix", which is really a list containing a function to
## set the value of the matrix, get the value of the matrix
## set the value of the inverted matrix, get the value of the inveted matrix


makeCacheMatrix <- function(mat = matrix()){
  
  #initialize a variable for inverted matrix
  invertedMat <- NULL
  
  #set the passed matrix to a new variable
  #assign NULL to the inverted matrix and cache the result
  set <- function(y) {
    mat <<- y
    invertedMat <<- NULL
  }
  
  #getter: return the passed matrix as is
  get <- function() mat
  
  #setter: sets the inverted matrix to a certain value
  #cache the new value of inverted matrix
  setInverse <- function(iMat) invertedMat <<- iMat
  
  #getter: gets the inverted value
  getInverse <- function() invertedMat
  
  #return the special 'matrix'
  #basically returns a list
  list( set = set, get = get, getInverse = getInverse, setInverse = setInverse)
  
}


##This function calculates the inverse of the special "matrix" created with the above function. 
##It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...){
  
  #set the inverted matrix with the value of inverted matrix present in the 
  #special 'matrix'
  invertedMat <- x$getInverse()
  
  #if the value of inverted matrix is non-null return it
  if(!is.null(invertedMat)){
    
    print("getting cached data")
    return(invertedMat)
  }
  
  #if the fetched value of inverted matrix is null
  #get the matrix
  data <- x$get()
  
  #compute the inverted matrix using the solve() function
  invertedMat <- solve(data)
  
  #cahce the computed value
  mat$setInverse(invertedMat)
  
  #return the computed value
  invertedMat
  
}
