
get_ds_pcc_alpha <-
function(log_alpha, mu0, p, m, n, p1, lmax, ss, sampling.p=0.5)
{
  n1=ceiling(n*sampling.p)
  n2=n-n1
  tau=2*mu0/(sqrt(1/n1+1/n2)) 

	alpha=exp(log_alpha)
	beta= pt( tau -qt(alpha/2,df=n-2, lower.tail=F) , df=n-2, lower.tail=F)

	num=mu0*m*(1-beta)
	denom=sqrt(lmax)*sqrt(m*(1-beta)+alpha*(p-m))
	
	Q_value=Q_func2(num,denom, p1=p1)
	pcc_value=sum(Q_value*c(p1, 1-p1))

	if (ss==F)
		pcc_value
	else
		c(pcc_value, Q_value)
}

