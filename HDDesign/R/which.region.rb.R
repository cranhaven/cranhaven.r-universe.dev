which.region.rb <-
function(r, beta)
{
	if (r>=1 || beta>=1)
		return(4)
	if (r<= rho(beta))
		return(0)
	if (is.region1(r, beta))
		return(1)
	else
		if(is.region2(r, beta))
			return(2)
		else
			return(3)
}
