## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
# Declaring a function with a single argument of class "Matrix"
{
m<-matrix()
# settin/resetting m to an empty object of class 'matrix'
        set<-function(y=matrix())
        #setting access to X in another environment
        {
                x<<-y
                m<<-matrix()
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
# creating a list of all elements necessary to store in cache
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
#declaring function that has the first argument as matrix and can take several others
{ m<-makeCacheMatrix(x)[3] #use the already calculated inverse from cache
        if(det(m)!=0) #check if it is not the default matrix
        {
                message("getting cached inverse")
                return(m)
        }
        mat<-as.matrix(makeCacheMatix(x)[2]) #if a new matrix is stored take and calculate its inverse
        m<-solve(mat)
        as.matrix(makeCacheMatix(x)[3])<-m
        m
}
