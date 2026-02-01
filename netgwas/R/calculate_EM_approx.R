#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

calculate_EM_approx = function(chain, y, Z, rho, Sigma, Theta, lower_upper, em_tol, em_iter, ncores=4, verbose=FALSE )
{
	c_em_iter = 1
	dif	<- 100
	p 	<- ncol(y)
	n	<- nrow(y)
	s <- proc.time()	
	while((c_em_iter < em_iter) && (dif >= em_tol ))
	{
		S_obj	<- calculate.R.approx.internal(y=y, Z=Z, lower_upper=lower_upper, Sigma= Sigma, ncores=ncores)
		Z		<- S_obj$Z
		S_gl	<- glasso(s=S_obj$ES, rho=rho, maxit=1000, penalize.diagonal=FALSE)	
		Theta <- (t(S_gl$wi) + S_gl$wi) / 2
		Sigma_new <- (t(S_gl$w) + S_gl$w) / 2
		sd_marginal <- sqrt(diag(Sigma_new))
		sd_marginal[abs(sd_marginal) < 1e-10] <- 1e-10			
		Sigma_new	<- diag(1/sd_marginal) %*% Sigma_new %*% diag(1/sd_marginal)
		Theta	<- Matrix(diag(sd_marginal) %*% Theta %*% diag(sd_marginal))	
		dif		<-	sum(abs(Sigma_new - Sigma)/p^2) 
		Sigma	<-  Sigma_new
		c_em_iter <- c_em_iter + 1
		rm(Sigma_new, sd_marginal, S_gl)
		gc()
	}

	results <- list()
	results$Theta	<- Theta
	results$Sigma	<- Sigma
	results$ES		<- S_obj$ES
	results$Z		<- Z
	results$rho		<- rho
	results$loglik	<- n/2 *(determinant(results$Theta, logarithm = TRUE)$modulus - sum(diag(results$ES %*%results$Theta)))

	return(results)
}
