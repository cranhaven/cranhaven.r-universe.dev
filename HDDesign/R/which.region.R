which.region <-
function(mu0, p, m, n)
{
	r=(n*mu0^2)/(2*log(p))
	beta=-log(m/p)/log(p)
	which.region.rb(r, beta)
}
