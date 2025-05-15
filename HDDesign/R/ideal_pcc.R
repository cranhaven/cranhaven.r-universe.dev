ideal_pcc <-
function(mu0, m, p1=0.5)
{
	kt=1/2*log((1-p1)/p1)
	pnorm((m*mu0^2-kt)/(sqrt(m)*mu0))*p1+pnorm((m*mu0^2+kt)/(sqrt(m)*mu0))*(1-p1)
}

