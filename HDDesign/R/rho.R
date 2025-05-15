rho <-
function(beta)
{
	if (0<=beta && beta<=1/2)
		return(0)
	if (1/2<beta && beta <=3/4)
		return(beta - 1/2)
	if (3/4<beta && beta <= 1)
		return((1-sqrt(1-beta))^2)
}
