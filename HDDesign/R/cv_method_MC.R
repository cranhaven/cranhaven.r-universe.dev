cv_method_MC <-
function(mu0, p, m, n, alpha_list, nrep, p1=0.5, ss=F, ntest, sampling.p=0.5)
{
	temp=sapply(rep(mu0, nrep), cv_method_single_MC, p, m, n, alpha_list, p1, ss, ntest,sampling.p) 

	if (ss==F)
             mean(temp) 
	else
	        apply(temp, 1, mean)
}
