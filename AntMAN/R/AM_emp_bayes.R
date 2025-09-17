#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################


#' compute the hyperparameters of an Normal-Inverse-Gamma distribution using an empirical Bayes approach
#'
#' This function computes the hyperparameters of a Normal Inverse-Gamma distribution using an empirical Bayes approach. 
#' More information about how these hyperparameters are determined can be found here: 
#' \emph{Bayes and empirical Bayes: do they merge?} \insertCite{petrone2012bayes}{AntMAN}.
#'
#'@param y The data y. If y is univariate, a vector is expected. Otherwise, y should be a matrix.
#'@param scEmu a positive value (default=1) such that marginally E(\eqn{\mu}) = \eqn{s^2}*scEmu, where \eqn{s^2} is the
#'sample variance.
#'@param scEsig2 a positive value (default=3) such that marginally E(\eqn{\sigma^2}) = \eqn{s^2}*scEsig2, where \eqn{s^2} is the
#'sample variance.
#'@param CVsig2 The coefficient of variation of \eqn{\sigma^2} (default=3).
#'  
#'@return an object of class \code{\link{AM_mix_hyperparams}}, in which hyperparameters \code{m0}, \code{k0},
#' \code{nu0} and \code{sig02} are specified. To understand the usage of these hyperparameters, please refer to
#' \code{\link{AM_mix_hyperparams_uninorm}}.

#'@export
AM_emp_bayes_uninorm = function(y,scEmu=1,scEsig2=3,CVsig2=3){
	n <- length(y)   ### sample size
	bary <- mean(y)  ### sample mean
	s2y <- var(y)    ### sample variance
	
	Emu <- bary
	Vmu <- s2y*scEmu
	Esig2 <- s2y/scEsig2
	Vsig2 <- CVsig2^2*Esig2^2
	
	m0    = Emu
	nu0   = 2*(Esig2)^2/Vsig2+4
	sig02 = Esig2*(nu0-2)/nu0
	k0    = sig02/Vmu * nu0/(nu0-2)
	
	return(AM_mix_hyperparams_uninorm(m0=m0,nu0=nu0,sig02=sig02,k0=k0))
	
}

