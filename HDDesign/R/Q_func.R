Q_func <-
function(tt, p1)
{
	k=1/2*log((1-p1)/p1)/tt
	c(pnorm(tt-k),  pnorm(tt+k))
}



Q_func2 <-
function(num,denom, p1)
{
	k=1/2*log((1-p1)/p1)

	c(pnorm((num-k)/denom),  pnorm((num+k)/denom))
}
