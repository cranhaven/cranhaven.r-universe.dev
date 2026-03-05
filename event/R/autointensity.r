#
#  event : A Library of Special Functions for Event Histories
#  Copyright (C) 1998, 1999, 2000, 2001 J.K. Lindsey
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public Licence as published by
#  the Free Software Foundation; either version 2 of the Licence, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public Licence for more details.
#
#  You should have received a copy of the GNU General Public Licence
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#  SYNOPSIS
#
# autointensity(times, window=NULL, maxlag=total/10,
#	ylab="Conditional probability", xlab="Lag",
#	main="Autointensity function", xlim=c(0,max(times)),
#	ylim=c(0,if(plotse)max(se1)else max(z$density)), lty=1,
#	plot=TRUE, plotse=TRUE, add=FALSE, ...)
#
#  DESCRIPTION
#
#    Plots the autointensity function of a point process

autointensity <- function(times, window=NULL, maxlag=max(times),
	ylab="Conditional probability", xlab="Lag",
	main="Autointensity function", xlim=c(0,max(times)),
	ylim=c(0,if(plotse)max(se1)else max(z$density)), lty=1,
	plot=TRUE, plotse=TRUE, add=FALSE, ...){
if(any(times<=0))stop("Times must be strictly positive")
n <- length(times)
total <- sum(times)
if(is.null(window))window <- total/n/2
#
# calculate histogram and standard errors
#
tmp <- cumsum(times)
times <- tmp[2:n]-tmp[1]
for(i in 3:n)times <- c(times,tmp[i:n]-tmp[i-1])
times <- times[times<maxlag]
z <- hist(times,plot=FALSE,breaks=seq(0,max(times+window),by=window))
z$density <- z$counts*total/(total-z$mids)/window/n
se1 <- ifelse(z$density>0,(sqrt(z$density)+2/sqrt(window*n))^2,0)
se2 <- sqrt(z$density)-2/sqrt(window*n)
se2 <- ifelse(z$density>0,ifelse(se2>0,se2,0)^2,0)
#
# plot if required
#
if(plot){
	if(add)plot(z$mids,z$density,type="l",lty=lty)
	else plot(z$mids,z$density,type="l",main=main,ylab=ylab,xlab=xlab,
		xlim=xlim,ylim=ylim,lty=lty,...)
	lines(xlim,rep(n/total,2))
	if(plotse){
		lines(z$mids,se1,lty=3)
		lines(z$mids,se2,lty=3)}}
invisible(list(mids=z$mids,density=z$density,se=rbind(se1,se2)))}
