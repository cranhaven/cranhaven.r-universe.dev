EXP.score <- 
function(LP.par, weight.vec, leg.mat){
#Function for part one of numerator
	EXP.num.1 <- function(weight, leg.mat){
		Ep.d <- ( t(leg.mat) %*% weight )/ nrow(leg.mat) ##Vector length m
		return(as.vector(Ep.d))
		}
#Function for part two of numerator
	EXP.num.2 <-function(c.vec, weight, leg.mat){
		Ep.d <- NULL
		for(j in 1:length(c.vec)){
			inner.part <- apply(leg.mat, 2, function(x) x*leg.mat[,j])
			v <-(t(inner.part)%*% weight)/nrow(leg.mat)
			Ep.d[j] <- as.vector(v) %*% c.vec
			}
		return(Ep.d)
	}
#Function for denominator
	EXP.denom <- function(c.vec, weight, leg.mat){
		Ep.d <- NULL
		Ep.d <- ( t(leg.mat) %*% weight )/ length(weight)
		denom <- as.vector(1 + as.vector(Ep.d) %*% c.vec) #scalar
		return(denom)
		}
#Final Calculation		
	vec.map <- (EXP.num.1(weight.vec, leg.mat) + 
					EXP.num.2(LP.par, weight.vec, leg.mat) ) / 
					EXP.denom(LP.par, weight.vec, leg.mat)
	return(vec.map)
}