## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m<-matrix()
        set<-function(y=matrix())
        {
                x<<-y
                m<<-matrix()
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { m<-makeCacheMatrix(x)[3]
        if(det(m)!=0) 
        {
                message("getting cached inverse")
                return(m)
        }
        mat<-as.matrix(makeCacheMatix(x)[2])
        m<-solve(mat)
        as.matrix(makeCacheMatix(x)[3])<-m
        m
}
