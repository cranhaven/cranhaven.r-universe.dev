#' To compute the correlation matrix in terms of hypersphere decomposition approach
#'
#' The correlation matrix is reparameterized via hyperspherical coordinates angle parameters for \cr 
#' trigonometric functions, 
#' and the angle parameters are referred to hypersphere (HS) parameters. In order to obtain the unconstrained estimation 
#' of angle parameters and to reduce the number of parameters for facilitating the computation, 
#' we model the correlation structures of the responses in terms of the generalized linear models 
#' @param w a design matrix is used to model the HS parameters as functions of subject-specific covariates. 
#' @param delta an \eqn{a \times 1} vector of unknown parameters to model the HS parameters.
#' @return a correlation matrix
#'
#' @author Kuo-Jung Lee <kuojunglee@ncku.edu.tw> 
#' @references{
#'   \insertRef{Zhang:etal:2015}{BayesRGMM} 
#'} 
#' @examples
#' \dontrun{
#' library(BayesRGMM)
#' rm(list=ls(all=TRUE))
#' T = 5 #time points
#' HSD.para = c(-0.5,  -0.3) #the parameters in HSD model
#' a = length(HSD.para)
#' w = array(runif(T*T*a), c(T, T, a)) #design matrix in HSD model
#' signif(CorrMat.HSD(w, HSD.para), 4)
#' }

CorrMat.HSD = function(w, delta)
{

	T = dim(w)[1]
	F.tmp = matrix(0, T, T)

	for(l in 1:T)
		for(m in 1:T)
			F.tmp[l, m] = sum(w[l, m, ]*delta)

	F.tmp = exp(F.tmp)*pi/(1+exp(F.tmp))

	F = matrix(0, T, T)
	F[1, 1] = 1
	for(l in 2:T)
		F[l, 1] = cos(F.tmp[l, 1])

	for(m in 2:(T-1))
		for(l in (m+1):T)
				F[l, m] = cos(F.tmp[l, m]) * prod(sin(F.tmp[l, 1:m-1]))

	for(m in 2:T)
		F[m, m] = prod(sin(F.tmp[m, 1:m-1]))

	Ri = F%*%t(F)

	Ri
}
