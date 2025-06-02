blkdiag <- function(A,B){
	
	if(is.null(A) & is.null(B)) C = NULL
	else if(!is.null(A) & is.null(B)) C = A
	else if(is.null(A) & !is.null(B)) C = B
	else{
		A = as.matrix(A); B = as.matrix(B)
		dimA = dim(A)
		dimB = dim(B)  
		C = array(0,dimA+dimB)
		if(prod(dimA)>0) C[1:dimA[1],1:dimA[2]] = A	
		if(prod(dimB)>0) C[dimA[1]+(1:dimB[1]),dimA[2]+(1:dimB[2])] = B
	}
	return(C)
		
}