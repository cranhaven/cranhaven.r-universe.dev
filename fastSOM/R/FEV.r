## Contents: 
#		forecast_error_variances

################################################################################
# calculate the forecast error variances
forecast_error_variances <- function(Sigma,A,N=dim(Sigma)[1],H=dim(A)[3])
{
# formerly called calcNenner
# Sigma is a covariance matrix, A is an array of MA coefficients
	res <- .Call(C_fev,Sigma,A,N,H) 
	names(res) <- dimnames(Sigma)[[1]]
	res
}

