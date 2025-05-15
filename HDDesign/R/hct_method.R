hct_method <-
function(mu0, p, m, n, hct, alpha0, nrep, p1=0.5, ss=F,sampling.p=0.5)
{
	temp=sapply(1:nrep, hct_method_single, mu0, p, m, n, hct, alpha0, p1, ss, sampling.p)
	if (ss==F)
		mean(temp)
	else
		apply(temp, 1, mean)
}
