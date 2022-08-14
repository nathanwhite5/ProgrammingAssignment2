## a) makeCacheMatrix first initializes two objects, 'x' and 'i'.'x' is initialized as a function argument as and is an empty numeric vector, and 'i' is set to NULL to be used later. 

## b) After this, the first of four instructions comes in the form of the function set() - taking the argument 'y'. set() has two commands: first assign the input argument (y) to the object 'x' in the parent environment, and then assign to 'i' in the parent environment the value of NULL. This clears any value of 'i' that has been cached prior to the execution of the subsequent running of the caching function 'cacheSolve'. The first two lines of code in set() do exactly the same thing as the first two lines in the main function. 

## c) The next instruction is the function get(), getting the value 'x' from the parent environment of makeCacheMatrix() since it isnt defined within get(). 

## d) Next, setinverse() assigns the input argument 'solve' (that produces the matrix's inverse) to the value of 'i' in the parent environment.  

## e) The final instruction getinverse() gets the inverse 'i'  from the parent environment set in the previous function function. 

## f) Lastly, the last bit of code assigns each of the aformentioned four functions as elements in a List(), naming them accordingly to be access each function by name with '$', returning them to the parent environment

makeCacheMatrix <- function(x = matrix()) { #a
  i <- NULL 
  set <- function(y){ #b
    x <<- y
    i <<- NULL
  }
  get <- function() x #c
  setinverse <- function(solve) i <<- solve #d
  getinverse <- function() i #e
  list(set = set, get = get, #f
       setinverse = setinverse,
       getinverse = getinverse)

}

## i) the cacheSolve starts with a single argument 'x' and an ellipsis that allows  additional arguments to be passed to the function via whats calling it.

## ii) The first instruction is to attempt to retrieve the inverse matrix from the object passed through the function via x$getinverse(). 

## iii) if there is already a inverse cached, it will return the message 'get cached data' and produce the the value 'i' to the parent environment. 

## iv) If this is the first time the particular matrix is passed into the object, 'i' will be NULL and !na.null(i) is FALSE, so cacheSolve retrieves the matrix from the input object via x$get() and places it in an object named 'data'. 

## v) the object 'data' containing the matrix is passed through the solve() function, calculating the inverse matrix. Note, this is the only place where solve() is executed, so makeCacheMatrix() is useless without cacheSolve()

## vi) the setinverse() function sets the inverse in the input object via x$setinverse()

## vii) finally the inverse is printed and the value of the inverse is returned to the parent environment 

cacheSolve <- function(x, ...) { #i
  i <- x$getinverse() #ii
  if(!is.null(i)){ #iii
    message("get cached data")
    return(i)
  }
  data <-x$get() #iv
  i <- solve(data,...) #v
  x$setinverse(i) #vi
  i #vii
}

#########################################

test <- matrix(1:4, 2 , 2) ## test matrix
test.cache <- makeCacheMatrix(test) ## pass test matrix into function
cacheSolve(test.cache) ## inverse produced and cached
cacheSolve(test.cache) ## message 'get cached data' produced upon subsequent run

