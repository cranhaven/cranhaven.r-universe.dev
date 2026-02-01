#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

element_S_j = function(j, lower_upper, mu=0, sigma=1)
{
  	delta1 <- (lower_upper$lower[ ,j] - mu) / sigma
  	delta2 <- (lower_upper$upper[ ,j] - mu) / sigma
  	tmp1 <- (dnorm(delta1) - dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  	EX <- mu + tmp1 * sigma
  	
  	delta1[delta1 < -1e+10] <- -1e+10
  	delta2[delta2 > 1e+10] <- 1e+10
  	tmp2 <- (delta1*dnorm(delta1) - delta2*dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  	EXX <- sigma^2 + mu^2 + sigma^2 * tmp2 + 2 * mu * sigma * tmp1
	rm(delta1, delta2, tmp1, tmp2)
	gc()
	
	return(list(EX=EX, EXX=EXX))
}

approx_method = function(y, Z, ES=NULL, rho = NULL, lower_upper=NULL, chain=1, ncores=4, em_tol=.001, em_iter=10 )
{
	obj <- glasso(s=ES, rho=rho[chain], maxit=1000, penalize.diagonal=FALSE)
	Theta <- Matrix((t(obj$wi) + obj$wi) / 2, sparse = TRUE)
	Sigma <- (t(obj$w) + obj$w) / 2
	sd_marginal  <- sqrt(diag(Sigma))
	sd_marginal[abs(sd_marginal) < 1e-10] <- 1e-10
	Sigma <- diag(1/sd_marginal) %*% Sigma %*% diag(1/sd_marginal)
	Theta <- diag(sd_marginal) %*% Theta %*% diag(sd_marginal)
	rm(obj, sd_marginal)
	gc()

	approx.method <- calculate_EM_approx(chain=chain, y=y, Z=Z, rho= rho[chain], Sigma=Sigma, Theta= Theta, lower_upper=lower_upper, em_tol=em_tol, em_iter = em_iter, ncores=ncores)

	invisible(return(approx.method))
}