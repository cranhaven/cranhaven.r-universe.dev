gen_sim_pvalue_null <-
function(p, m, alpha0)
{
	nn=ceiling(p*alpha0) 
	u=rbeta(1, nn, p-m+1-nn)
	v=runif(nn-1)*u
	c(v, u)
}
