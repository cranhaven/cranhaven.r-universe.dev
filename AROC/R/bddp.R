bddp <-
function(y, X, alpha = 1, m, S, nu, psi, a, b, nsim, L, scale = TRUE) {
  yt <- y  
  if (scale==TRUE) {yt <- y/sd(y)}
	n <- length(y)
	k <- ncol(X)

	p <- ns <- rep(0,L)
	v <- rep(1/L,L)
	v[L] <- 1

	beta <- matrix(0, nrow = L, ncol = k)
	aux <- try(solve(t(X)%*%X)%*%t(X)%*%yt, silent = TRUE)
	if(!inherits(aux, "try-error")) {
		for(l in 1:L) {
			beta[l,] <- aux   
		}
	}
	
	tau <- rep(1/var(yt),L)
	prop <- prob <- matrix(0, nrow = n, ncol = L)
  
	P <- Tau <- Sigma2 <- matrix(0, nrow = nsim, ncol = L)
	Beta <- Beta1 <- array(0,c(nsim,L,k))
	Beta[1,,] <- beta
	Tau[1,] <- tau
  
	mu <- matrix(0, nrow = nsim, ncol = k)
	Sigmainv <- array(0, c(nsim,k,k))
	mu[1,] <- mvrnorm(1, mu = m, Sigma = S)
	Sigmainv[1,,] <- rWishart(1, df = nu, solve(nu*psi))

	for(i in 2:nsim) {
		cumv <- cumprod(1-v)
		p[1] <- v[1]
		for(l in 2:L) {
			p[l] <- v[l]*cumv[l-1]
		}
		for(l in 1:L) {
			prop[,l] <- p[l]*dnorm(yt,mean=X%*%beta[l,],sd = sqrt(1/tau[l]))
		}
    
		prob <- prop/apply(prop,1,sum)
		z <- rMultinom(prob,1)
		P[i,] <- p
    
		for(l in 1:L) {
			ns[l] <- length(which(z == l))
		}
    
		for(l in 1:(L-1)) {
			v[l] <- rbeta(1, 1 + ns[l],alpha+sum(ns[(l+1):L]))
		}
    
		for(l in 1:L) {
			tX  <- matrix(t(X[z == l, ]),nrow = k, ncol = ns[l])
			V <- solve(Sigmainv[i-1,,]+tau[l]*tX%*%X[z == l,])
			mu1 <- V%*%(Sigmainv[i-1,,]%*%mu[i-1,]+tau[l]*tX%*%yt[z == l])
			Beta1[i,l,] <- Beta[i,l,] <- beta[l,] <- mvrnorm(1, mu = mu1, Sigma = V)
            if (scale == TRUE) {
                Beta1[i,l,] <- sd(y)*Beta[i,l,]
            }
      
			Tau[i,l] <- tau[l] <- rgamma(1, shape = a + (ns[l]/2), rate = b + 0.5*(t(yt[z==l]-X[z==l,]%*%t(beta[l,,drop=FALSE]))%*%(yt[z==l]-X[z==l,]%*%t(beta[l,,drop=FALSE]))))
            
			Sigma2[i,l] <- 1/Tau[i,l]
            if (scale == TRUE){
                Sigma2[i,l] <- var(y)*(1/Tau[i,l])
            }
		}
    
		Vaux <- solve(solve(S)+L*Sigmainv[i-1,,])
		if(k == 1) {
            meanmu <- Vaux%*%(solve(S)%*%m+Sigmainv[i-1,,]%*%sum(Beta[i,,]))
        } else {
            meanmu <- Vaux%*%(solve(S)%*%m+Sigmainv[i-1,,]%*%t(t(apply(Beta[i,,],2,sum))))
        }
		mu[i,] <- mvrnorm(1, mu = meanmu, Sigma = Vaux)
    
		Vaux1 <- 0
		for(l in 1:L) {
			Vaux1 <- Vaux1+(Beta[i,l,]-mu[i,])%*%t((Beta[i,l,]-mu[i,]))  
		}
		Sigmainv[i,,] <- rWishart(1,nu+L,solve(nu*psi+Vaux1))	    
	}
	return(list(P,Beta1,Sigma2))
}
