hct_method_MC <-
function(mu0, p, m, n, hct, alpha0, nrep, p1=0.5, ss=F, ntest, sampling.p)
{
	temp=sapply(1:nrep, hct_method_single_MC , mu0, p, m, n, hct, alpha0, p1, ss, ntest, sampling.p)

	if (ss==F)
           mean(temp) 
	else
		apply(temp, 1, mean)
}
