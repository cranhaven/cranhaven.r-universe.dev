initial.convex <-
function(A,B,lambda,K,nu=1,epsilon=5e-3,maxiter=1000,trace=FALSE){
	p <- nrow(B)
	eigenB <- eigen(B)
	sqB <- eigenB$vectors%*%sqrt(diag(pmax(eigenB$values,0)))%*%t(eigenB$vectors)	
	tau <- 4*nu*eigenB$values[1]^2	
	criteria <- 1e10
	i <- 1
	# Initialize parameters
	H <- Pi <- oldPi <-  diag(1,p,p)
	Gamma <- matrix(0,p,p)
    	# While loop for the iterations
    	while(criteria > epsilon && i <= maxiter){
        Pi <- updatePi(B,sqB,A,H,Gamma,nu,lambda,Pi,tau)

		H <- updateH(sqB,Gamma,nu,Pi,K)
		Gamma <- Gamma + sqB%*%Pi%*%sqB-H	
        criteria <- sqrt(sum((Pi-oldPi)^2))
        oldPi <- Pi
        i <- i+1
        if(trace==TRUE)
        {
 		    print(i)
			print(criteria)
        }
	}
	return(list(Pi=Pi,H=H,Gamma=Gamma,iteration=i,convergence=criteria))

}
