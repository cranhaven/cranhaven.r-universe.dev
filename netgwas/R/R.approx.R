#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

element_S <- function( lower, upper, mu=0, sigma=1)
{
  	delta1 <- (lower - mu) / sigma
  	delta2 <- (upper - mu) / sigma
  	tmp1 <- (dnorm(delta1) - dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  	EX <- mu + tmp1 * sigma
  	
  	delta1[delta1 < -1e+10] <- -1e+10
  	delta2[delta2 > 1e+10] <- 1e+10
  	tmp2 <- (delta1*dnorm(delta1) - delta2*dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  	EXX <- sigma^2 + mu^2 + sigma^2 * tmp2 + 2*mu*sigma*tmp1
	rm(delta1, delta2, tmp1, tmp2 )
	gc()
	
	return(list(EX=EX, EXX=EXX))
}

cond_N <- function(j, Sigma, Z , Z_new, diag_element, lower_upper)
{
	p <- ncol(Sigma)
	tmp <- matrix(Sigma[j, -j], 1, p-1)
	tmp1 <- solve(Sigma[-j, -j])

	mu <- tmp %*% tmp1 %*% t(Z[, -j])				
	mu <- as.vector(mu)		 
	sigma <- Sigma[j, j] - tmp %*% tmp1 %*% t(tmp)
	sigma <- sqrt(sigma)  		
		  
	obj <- element_S( lower= lower_upper$lower[ ,j], upper= lower_upper$upper[ ,j], mu=mu, sigma=sigma)      

	Z_new <- obj$EX
	diag_element <- mean(obj$EXX)
	
	rm(tmp, tmp1, mu, sigma, obj )
	gc()
	
	return(list(Z_new=Z_new, diag_element=diag_element))
}

calculate.R.approx.internal <- function(y, Z, lower_upper, Sigma, ncores = NULL )
{      
	if(is.null(ncores)) ncores = detectCores() - 1
	p <- ncol(y)
	n <- nrow(y) 
	Z_new <- rep(0, n)
	
	if(ncores > 1){
		cl <- makeCluster(ncores)
		Sigma <- Sigma
		Z <- Z
		cond_norm <- parLapply(cl = cl, 1:p, function(j) { 
			cond_N(j, Sigma=Sigma, Z=Z , Z_new=Z_new, diag_element=diag_element, lower_upper=lower_upper ); 
		})
		stopCluster(cl)
	}else{
		cond_norm <- lapply(1:p, function(j){ cond_N(j, Sigma, Z , Z_new, diag_element, lower_upper); } )
	}
	
	Z_new <- do.call(cbind, lapply(1:p, function(x) cond_norm[[x]]$Z_new ))
	diag_element <- sapply(1:p, function(x) cond_norm[[x]]$diag_element)
	
	ES <- t(Z_new) %*% Z_new / n
	diag(ES) <- diag_element

	output <- list()
	output$ES <- ES
	output$Z <- Z_new
	
	rm(lower_upper, ES, Z_new )
	return(output)         
}


R.approx = function(y, Z = NULL, Sigma=NULL, rho = NULL ,  ncores = NULL )
{
	p <- ncol(y)
	n <- nrow(y) 
	if(is.null(ncores)) ncores= detectCores()
	lower.upper <- lower.upper(y)
	
	if( is.null(Z) )
	{
		## Initialize S
		Z <- matrix(0, n, p)
		diag_element <- rep(0, p)
		if(ncores > 1)
		{
			cl <- makeCluster(ncores)
			tmp2 <- parLapply(cl = cl, 1:p, function(i) { 
			element_S_j(i, lower.upper ); 
			})
			stopCluster(cl)
		}else{
			tmp2 <- lapply(1:p, function(i){ element_S_j(i, lower.upper );})
		}
		Z <-  do.call(cbind, lapply(1:p, function(x) tmp2[[x]]$EX ))
		diag_element <- unlist(lapply(1:p, function(x)mean(tmp2[[x]]$EXX)))
		ES <- t(Z) %*% Z / n
		diag(ES) <- diag_element
		rm(tmp2, diag_element)	 
		gc() 
	}
	
	if(is.null(Sigma))
	{
		if(is.null(rho)) rho = max(max(ES),-min(ES))
		obj <- glasso(s=ES, rho=rho, maxit=1000, penalize.diagonal=FALSE)
		Sigma <- (t(obj$w) + obj$w) / 2
		sd_marginal  <- sqrt(diag(Sigma))
		sd_marginal[ abs(sd_marginal) < 1e-10 ] <- 1e-10
		Sigma <- diag(1/sd_marginal) %*% Sigma %*% diag(1/sd_marginal)
	}
	
	calculate.R.approx.internal( y, Z, lower.upper, Sigma, ncores = ncores)
}
