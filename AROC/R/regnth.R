regnth <-
function(y,X,m0,S0,nu0,psi0,a,b,nsim,scale=TRUE) {
	yt=y
  if(scale==TRUE){
  yt <- y/sd(y)
  }
  n <- length(y)
	k <- ncol(X)
	   
	beta <- beta1 <- matrix(0, nrow = nsim, ncol = k)
	sigma2 <- sigma21 <- numeric(nsim)
	mu0 <- matrix(0, nrow = nsim, ncol = k)
	V0inv <- array(0, c(nsim,k,k))  

	bols <- solve(t(X)%*%X)%*%(t(X)%*%yt)
	e <- yt-X%*%bols
	s2 <- (t(e)%*%e)/(n-k)
	beta[1,] <-bols
	sigma2[1] <- s2
	if(scale == TRUE){
	beta1[1,] <- sd(y)*beta[1,]
	sigma21 <- var(y)*sigma2[1]
	}
	mu0[1,] <- rep(0,k)
	V0inv[1,,] <- solve(100*diag(k))  
	  
	for(i in 2:nsim) {
		V1 <- solve(V0inv[i-1,,]+(1/sigma2[i-1])*t(X)%*%X)  
		mu1 <- V1%*%(V0inv[i-1,,]%*%mu0[i-1,]+(1/sigma2[i-1])*t(X)%*%yt)
		beta1[i,] = beta[i,] <- mvrnorm(1, mu = mu1, Sigma = V1)
		if(scale==TRUE){
		beta1[i,] <- sd(y)*beta[i,]   
		}    
		
		a1 <- a+(n/2)
		b1 <- b+0.5*(t(yt-X%*%beta[i,])%*%(yt-X%*%beta[i,]))
		sigma21[i] = sigma2[i] <- 1/rgamma(1, shape = a1, rate = b1)
		if(scale==TRUE){
		sigma21[i] <- var(y)*sigma2[i]  
		}
		Vaux <- solve(solve(S0) + V0inv[i-1,,])
		mu0[i,] <- mvrnorm(1, mu = Vaux%*%(V0inv[i-1,,]%*%t(t(beta[i,]))+solve(S0)%*%m0), Sigma = Vaux)
		    
		V0inv[i,,] <- rWishart(1, df = nu0+1, solve(nu0*psi0 + (beta[i,]-mu0[i,])%*%(t(beta[i,]-mu0[i,]))))
	    
	}
	return(list(beta1,sigma21))
}
