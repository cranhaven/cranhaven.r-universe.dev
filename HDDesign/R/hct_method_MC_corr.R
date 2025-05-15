hct_method_MC_corr <-
function(mu0, p, m, n, hct, alpha0, nrep, p1=0.5, ss=FALSE, ntest, pcorr, chol.rho,sampling.p)
{

	temp=sapply(1:nrep, hct_method_single_MC_corr, mu0, p, m, n, hct, alpha0, p1, ss, ntest, pcorr, chol.rho,sampling.p)
	if (ss==FALSE)
            mean(temp)
	else
		apply(temp, 1, mean)
}
