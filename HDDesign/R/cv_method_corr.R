cv_method_corr <-
function(mu0, p, m, n, alpha_list, nrep, p1=0.5, ss=F, pcorr, chol.rho, sampling.p=0.5)
{
	temp=sapply(rep(mu0, nrep), cv_method_single_corr, p, m, n, alpha_list, p1, ss, pcorr, chol.rho, sampling.p)

	if (ss==F)
		mean(temp[,1])
	else
		apply(temp, 1, mean)

}
