## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##function that caches the inverse of a matrix

makeCahceMatrix <- function (x = matrix()){
  inv <- NULL
  set <- function(y){ #defining the set() function
    x<<-y # we use this to a value the variable in the parent environment
    inv <<- NULL #essentially clears the cahce. also in the parent environment
  }
  get <- function() x #x is defined above and R uses lexical scoping to retrieve its value
  setInverse <- function(solveMatrix) inv <<- solveMatrix #solve is a function that returns the inverse of, in this case, a matrix
  getinverse <- function() inv            # the value is set in the parent environment
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getinverse) #assigns names for the set(), get(), setInverse() and getInverse() functions
}

makeCacheMatrix(x)

cacheSolve <- function(x,...){
  inv<-x$getInverse() #naming the list functions above allows us to refer to them by $name as opposed to [[]]
  if(!is.null(inv)){     #checks if there is a value in the cache
    message("fetching cached data")
    return(inv)
  }
  mat<-x$get() #retrieves and assigns the value of x
  inv<-solve(mat, ...) #calculates the inverse
  x$setInverse(inv)  #sets the inverse in the cache. acts as the ELSE for the IF above
  inv #returns the cached inverse
}