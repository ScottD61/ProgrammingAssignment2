## Write a short comment describing this function

#The first function, makeVector creates a special "vector", which is really a list containing a function to
#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the mean
#4. get the value of the mean


> #Create matrix
> x <- matrix(rnorm(25), nrow = 5)
> #Create CacheMatrix
> makeCacheMatrix <- function(x = matrix()) {
#inv stores cached inverse matrix
+   inv <- NULL
#Set matrix
+   set <- function(y) {
+     x <<- y
+     inv <<- NULL
+   }
#Get matrix
+   get <- function() x
#Set inverse
+   setinverse <- function(inverse) inv <<- inverse
#Get inverse 
+   getinverse <- function() inv
#Return matrix
+   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
+ }
> 
> # The following function returns the inverse of the matrix. It first checks if
> # the inverse has already been computed. If so, it gets the result and skips the
> # computation. If not, it computes the inverse, sets the value in the cache via
> # setinverse function.
> 
> # This function assumes that the matrix is always invertible.
> cacheSolve <- function(x, ...) {
+   inv <- x$getinverse()
#Return inverse matrix if already calculated
+   if(!is.null(inv)) {
+     message("getting cached data.")
+     return(inv)
+   }
#Calculate inverse of matrix
+   data <- x$get()
+   inv <- solve(data)
#Cache inverse of matrix 
+   x$setinverse(inv)
+   inv
+ }

 #Create cacheMatrix()
> cx <- makeCacheMatrix(x) 

> #Return matrix
> cx$get() 
           [,1]       [,2]        [,3]        [,4]        [,5]
[1,]  0.1914453  1.5331059  0.55577289 -0.08515355 -0.02195232
[2,] -0.4488061 -0.7914014 -0.06361339  1.35098732  0.77144839
[3,]  0.1862152 -0.9110498  0.03466335 -0.28474755 -0.15610968
[4,] -0.2003549 -0.0952676  0.24789266  1.52806642 -1.63233932
[5,] -0.3078116  0.3689981 -0.65119649 -1.60121217  0.60056568

> #Return inverse matrix 
> cacheSolve(cx)
            [,1]       [,2]       [,3]       [,4]       [,5]
[1,] -2.32799092 -2.0253541 -3.0261047 -1.5744604 -2.5494405
[2,] -0.14091285 -0.3343951 -1.1096852 -0.1209113 -0.1926945
[3,]  2.87777203  1.5975218  3.8888931  0.8160611  1.2820424
[4,] -0.74147487 -0.2247774 -1.3414603 -0.2546522 -0.7792098
[5,]  0.03688243  0.3002965 -0.2289966 -0.5267655 -0.2105720
