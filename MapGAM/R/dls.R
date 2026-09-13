#******************************************************************************
#
# Calculating Derivatives of Partial Likelihood
# Copyright (C) 2016, The University of California, Irvine
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this library??? if not, <http://www.gnu.org/licenses/>.
#
#*******************************************************************************
dls <- function(Y,X,which,eta,span=0.5,adjust=TRUE){
	time = Y$time; event = Y$event;
	X = as.matrix(X)
	N <- length(time)
	lam <- exp(eta)
	points <- time[event==1]
	points <- points[(!duplicated(points))]
	dl <- dl2 <- rep(0,N)
	l <- 0
	for (i in 1:length(points)){
		active <- (time==points[i]&event==1)
		nactive <- (time>points[i]|(time==points[i]&event==0))
		d <- sum(active)
		ii <- 1:d
		dl.2 <- ii/d*sum(lam[active])+sum(lam[nactive])
		l <- l+sum(eta[active])-sum(log(dl.2))
		dl <- dl-active*sum((ii/d)/dl.2)-nactive*sum(1/dl.2)
		dl2 <- dl2+active*sum(((ii/d)/dl.2)^2)+nactive*sum(1/dl.2^2)
	}           
	dl2 <- dl*lam + dl2*lam^2
	dl <- dl*lam + event
	
	## Estimate the expected second derivatives
	if(adjust)
		fit <- gam(dl2~X+lo(X[,which],span=span,degree=1))
	else
		fit <- lm(dl2~X)
	
	dl2 <- fit$fitted.values
	return(list(deltaeta=-dl/dl2,w=-1/dl2,l=l))
}
