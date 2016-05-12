# This script consists of two functions that work together to calculate and cache
# the Inverse of a given Matrix.
#
# These functions are based on the example functions for Coursera's
# "Programming in R" Assignment 2 examples found at:
# https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping
#
# Sorry for my longwinded comments. I'm trying to explain it to myself.

# makeCacheMatrix(): This function takes a matrix as it's argument and returns
# a "matrix object" (my own terminology, probably incorrect) 
# with 4 callable functions defined by makeCachMatrix.
# So if M was an invertible matrix, the code:
#
# example <- makeCacheMatrix(M)
#
# would create a "Matrix Object" called "example". Then the code:
#
# example$get() 
#
# would call example's "get" funciton to retrieve the Matrix value.



makeCacheMatrix <- function(M = matrix(numeric(0),0,0)) {
      # define "i" variable to store the matrix inverse
      i <- NULL
      
      # set: function that caches the value of the given matrix 'M'
      # and sets the inverse to NULL.  
      set <- function(y) {
        M <<- y
        i <<- NULL
      }
      # get: one line functionreturns the value of matrix "M"
      get <- function() M
      
      # setinv:  one line function that caches the "inverse of M" matrix. 
      # setinv is called in CacheSolve.
      setinv <- function(inv) i <<- inv
      
      # getinv: one line function returns the "inverse of M" matrix
      getinv <- function() i
      
      # The following list is what is returned by makeCacheMatrix. 
      # Returning this list of functions creates the callable functions
      # in the parent environment.
      # If I understand this correctly it is a method for creating Objects in R.
      list(set = set, 
           get = get,
           setinv = setinv,
           getinv = getinv)
}
# CacheSolve():  This function takes a "Matrix Object" defined by makeCacheMatrix.
# The Function returns the inverse of the Matrix stored in the Matrix Object.
#
# so if the "matrix object" 'example' was defined as discussed above, the code:
#
# cacheSolve(example)
#
# would retrieve the inverse of M from the cache if 
# it has been previously calculated
# And it would calculate the inverse, cache it, and return it if 
# it has not been previously calculated

cacheSolve <- function(matrix_obj, ...) {
      # First retrieve the value stored for the inverse from the cache
      # if the inverse has not been calculated yet this will be NULL
      i <- matrix_obj$getinv()
      
      #if the retrieved value is not null, then return that value.
      if(!is.null(i)) {
          message("getting cached data")
          return(i)
      }
      #Otherwise, if the retrieved value stored in i IS null, then we need to
      # calculate the inverse.
      # So first retrieve the matrix from the object
      matrix <- matrix_obj$get()
      
      # then compute the inverse using "solve"...
      i <- solve(matrix, ...)
      
      # cache the inverse in the matrix object...
      matrix_obj$setinv(i)
      
      # and return the inverse and we're done.
      i
}
      