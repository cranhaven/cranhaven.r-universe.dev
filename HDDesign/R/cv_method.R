cv_method <-
function(mu0, p, m, n, alpha_list, nrep, p1=0.5, ss=F, sampling.p=0.5)
{
	temp=sapply(rep(mu0, nrep), cv_method_single, p, m, n, alpha_list, p1, ss, sampling.p)

	if (ss==F)
		mean(temp)
	else
		apply(temp, 1, mean)

}
