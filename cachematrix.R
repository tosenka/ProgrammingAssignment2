### Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
# set the value of the matrix;
# get the value of the matrix;
# set the inverse of a matrix;
# get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y) {
	x<<-y
	m<<-NULL
}
	get<-function()x 
	setinverse<-function(solve) m<<-solve
	getinverse<-function() m
	list(set=set, get=get, 
	setinverse=setinverse,
	getinverse=getinverse)
}
	


## This function computes the inverse of the special "matrix"  
# returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Check if 'x' matrix is invertible
        m_orig<-x$get()
        # Check if 'x' matrix is square
        if (nrow(m_orig)==ncol(m_orig)) {
                # Check if the determinant of 'x' does not equal to Zero
                if (det(m_orig)!=0) {                
                ## Return a matrix that is the inverse of 'x'
	                m<-x$getinverse()
	                if(!is.null(m)) {
	                        message("getting cached data")
	                        return(m)
                        }
	                data<-x$get()
	                m<-solve(data,...)
	                x$setinverse(m)
	                m
                }
                else {
                        message ("determinant is Zero")
                }
        }  
        else {
                message ("matrix is not square")
        }        
#                return(m_orig)
}
