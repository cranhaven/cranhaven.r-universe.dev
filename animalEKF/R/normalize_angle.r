normalize_angle <- function(theta) {
	#needed otherwise total lack of accuracy in modulus
	theta <- pmin(pmax(theta, -1e15), 1e15)
	
	theta <- sign(theta)*(abs(theta)%%(2*pi))
	if (any(theta[ ! is.na(theta) ] < -pi)) {  theta[ (theta < -pi) & (! is.na(theta)) ] <- theta[ (theta < -pi) & (! is.na(theta))] +(2*pi) }
	if (any(theta[ ! is.na(theta) ] >  pi)) {  theta[ (theta >  pi) & (! is.na(theta)) ] <- theta[ (theta > pi) & (! is.na(theta))] -(2*pi) }
	theta 
}
