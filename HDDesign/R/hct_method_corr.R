hct_method_corr <-
function(mu0, p, m, n, hct, alpha0, nrep, p1=0.5, ss=F, pcorr, chol.rho, sampling.p=0.5)
{
	temp=sapply(1:nrep, hct_method_single_corr, mu0, p, m, n, hct, alpha0, p1, ss, pcorr, chol.rho, sampling.p)
	if (ss==F){
	     mean(temp)  }
	else{
	     apply(temp, 1, mean) }
}
