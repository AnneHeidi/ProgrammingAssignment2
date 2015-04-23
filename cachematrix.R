##### Assignment 2 - Lexical Scoping and caching
####################

## Cache: a Place to store something temporarily in a computing environment

## overall description of what the functions do: 
    # MakeCacheMatrix makes a list containing a function to set the value of a matrix (x), get the value of the matrix
    # and to Set the value of the inversematrix (inverseM) and get the value of the inversematrix 


## makeCacheMatrix is a list of subfunctions to use
# The aim of  the  function makeCacheMatrix is to change the matrix
# setting variable invert to NULL, ‘NULL’ is often returned by expressions and functions whose value is undefined. 
# set is a function that _changes_ the matrix stored in the parent function (makeCacheMatrix)
# x <<- y substitutes x with y in the parent function (x <- y would only substitue x with y in the subfunction)
# invert is set to be NULL globally (i.e the parent environment, hence the makeCacheMatrix)
# get is function which returns the matrix stored in the parent function (makeCacheMatrix) 
# getinverseM is a function which gets the inverse matrix from the makeCacheMatrix
# list(): enlisting and storing the four subfunctions from the makeCacheMatrix


makeCacheMatrix <- function(x = matrix()) {        
        invert <- NULL                             
        
        set <- function(y){                        
            x <<- y                                
            invert <<- NULL                        
        }
        
        get <- function() {
            x 
        }                                         
        
        setinverseM <- function(inverseM) { 
            invert <<- inverseM      
        }                                          
        getinverseM <- function() {
            invert        
        }                                          
        list(
            set = set, 
            get = get,                             
            setinverseM = setinverseM,
            getinverseM = getinverseM
            )

}

 
# The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix 
# If the inverse has already been calculated (and the matrix has not changed), 
#   then the cacheSolve should retrieve the inverse matrix from the makeCacheMatrix
# ... should be used with care. Parameters given in ... can alter the results without altering the matrice which again can yield the wrong output
# invert subsets the getinverseM function from x which is given in the makeCacheMatrix
# ‘is.null’ returns ‘TRUE’ if its argument is ‘NULL’ and ‘FALSE’ otherwise, in this case i = NULL indicates cached data is present                                         
# When invert !is.null the message "getting cached data" appears while this function fetches cached data
# x$get() gives the variable data the original matrix before inversion
# Then the cacheSolvefunction is subsetting (and "solving") the setinverseM function of invert from x (the matrix) given in the makeCacheMatrix
# Finally cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...){                     
    invert <- x$getinverseM()                           
    if(!is.null(invert)) {                          
        message("getting cached data")              
        return(invert)                              
    data <- x$get()                                 
    invert <- solve(data, ...)                                            
    x$setinverseM(invert)                           
    invert                                          
}

## To sum up
# The first time we run through these functions invert is set as NULL 
# Then the matrice is inverted (invert <- solve....) and setinverseM sets this matrice. 
# When we repeat this and reach invert <- x$getinverseM() invert is !is.null and getinverseM gets this matrice (from the cache)
# in other words, we jump solve step and just uses the inverse matrice available from the cache

## just an example:
##########
# run function on a 2000 x 20000 matrix rnorm.
# foo is a list of the functions enlisted insed the makeCacheMatrix function
foo <- makeCacheMatrix(matrix(rnorm(2000**2), 2000, 2000))

# run to get inverse of matrix
cacheSolve(foo)

# run again to see that it runs faster and is "getting cached data"

# to see the original matrix by subsetting the getfunction in foo
foo$get 


