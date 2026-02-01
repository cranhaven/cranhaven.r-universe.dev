#-------------------------------------------------------------------------------#
# Package: Network-Based Genome-Wide Association Studies                        #
# Author: Pariya Behrouzi                                                       #
# Emails: <pariya.Behrouzi@gmail.com>                                           #
# Date: Nov 21th 2017                                                           #
#-------------------------------------------------------------------------------#

element_S <- function( lower, upper, mu=0, sigma=1)
{
  	delta1 <- (lower - mu) / sigma
  	delta2 <- (upper - mu) / sigma
  	tmp1 <- (dnorm(delta1) - dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  	EX <- mu + tmp1 * sigma
  	
  	delta1[delta1 < -1e+10] <- -1e+10
  	delta2[delta2 > 1e+10] <- 1e+10
  	tmp2 <- (delta1*dnorm(delta1) - delta2*dnorm(delta2)) / (pnorm(delta2) - pnorm(delta1))
  	EXX <- sigma^2 + mu^2 + sigma^2 * tmp2 + 2*mu*sigma*tmp1
	rm(delta1, delta2, tmp1, tmp2 )
	gc()
	
	return(list(EX=EX, EXX=EXX))
}