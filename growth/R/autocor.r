#
#  growth : A Library of Normal Distribution Growth Curve Models
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
#     pergram(y)
#     corgram(y, wt=1, add=FALSE, lty=1, xlim=NULL, ylim=NULL, xlab=NULL,
#		ylab=NULL, main=NULL)
#
#  DESCRIPTION
#
#    Functions to compute and plot correlograms and periodograms

### periodogram functions
###


#' Calculate and Plot a Periodogram
#' 
#' \code{pergram} calculates the values of a periodogram, \code{plot.pergram}
#' plots it, and \code{plot.cum.pergram} plots the corresponding cumulative
#' periodogram.
#' 
#' 
#' @aliases pergram plot.pergram plot_cum plot_cum.pergram
#' @param y A time series vector.
#' @param add If TRUE, adds a new periodogram to an existing plot.
#' @param x Plotting parameters
#' @param lty Plotting parameters
#' @param xlab Plotting parameters
#' @param ylab Plotting parameters
#' @param main Plotting parameters
#' @param ylim Plotting parameters
#' @param ... Plotting parameters
#' @return \code{pergram} prints and returns a two-column matrix of class,
#' \code{pergram}, containing the periodogram.
#' @author J.K. Lindsey
#' @keywords hplot
#' @examples
#' 
#' y <- rnorm(100)
#' print(z <- pergram(y))
#' plot(z)
#' plot_cum(z)
#' 
#' @export
pergram <- function(y){
	ll <- length(y)
	len <- trunc(ll/2)
	len1 <- if(ll==2*len)len+1 else len+2
	fc <- fft(y)
	z <- cbind(2*pi*(1:len)/ll,
		(Re(fc[ll:len1])^2+Im(fc[ll:len1])^2)*ll/4/pi)
	class(z) <- "pergram"
	z}

#pergram <- function(y){
#	ll <- length(y)
#	len <- trunc(ll/2)
#	fc <- matrix(0,ncol=2,nrow=ll)
#	for(i in 1:ll){
#		a <- 2*pi*i*(0:(ll-1))/ll
#		fc[i,1] <- sum(y*sin(a))
#		fc[i,2] <- sum(y*cos(a))}
#	invisible(cbind(2*pi*(1:len)/ll,(fc[,1]^2+fc[,2]^2)*ll/4/pi))}
#' @describeIn pergram Plot method
#' @export 
plot.pergram <- function(x, add=FALSE, lty=1, xlab="Frequency",
	ylab="Periodogram", main="Periodogram", ylim=c(0,max(po[,2])), ...){
  y <- x
	if(inherits(y,"pergram"))po <- y
	else po <- pergram(y)
	if(add)lines(po[,1],po[,2],lty=lty)
	else plot(po[,1],po[,2],xlim=c(0,3.6),xlab=xlab,ylab=ylab,type="l",
		lty=lty,ylim=ylim)
	invisible(po)}

#' @export 
plot_cum <- function(x, ...) UseMethod("plot_cum")

#' @describeIn pergram Plot_cum method
#' @export 
plot_cum.pergram <- function(x, xlab="Frequency", ylab="Periodogram",
	main="Cumulative periodogram",
	ylim=c(0,max(cpo+1.358/(a+0.12+0.11/a))), ...){
  y <- x
	if(inherits(y,"pergram")){
		len <- 2*dim(y)[1]
		po <- y}
	else {
		len <- length(y)
		po <- pergram(y)}
	cpo <- cumsum(po[,2])
	cpo <- cpo/max(cpo)
	a <- sqrt(len)
	pa <- 2*pi*(1:length(cpo))/len
	x <- (1:dim(po)[1])/dim(po)[1]
	plot(po[,1],cpo,xlim=c(0,3.6),xlab=xlab,ylab=ylab,type="l",
		ylim=ylim,main=main)
	lines(po[,1],x+1.358/(a+0.12+0.11/a),lty=3)
	lines(po[,1],x-1.358/(a+0.12+0.11/a),lty=3)}
###
### correlogram function
###


#' Calculate and Plot a Correlogram
#' 
#' \code{corgram} calculates the values of a correlogram (autocorrelation
#' function or ACF) and plots it.
#' 
#' 
#' @param y A time series vector.
#' @param maxlag Maximum number of lags for which the correlation is to be
#' calculated.
#' @param partial If TRUE, the partial autocorrelation function (PACF) is
#' plotted.
#' @param wt Indicator vector with zeros for values to be ignored.
#' @param add If TRUE, adds a new correlogram to an existing plot.
#' @param xlim Plotting parameters
#' @param lty Plotting parameters
#' @param xlab Plotting parameters
#' @param ylab Plotting parameters
#' @param main Plotting parameters
#' @param ylim Plotting parameters
#' @param ... Plotting parameters
#' @return \code{corgram} returns a two-column matrix containing the (partial)
#' correlogram coordinates.
#' @author J.K. Lindsey
#' @keywords hplot
#' @examples
#' 
#' y <- rnorm(100)
#' corgram(y)
#' corgram(y, partial=TRUE)
#' @export
corgram <- function(y, wt=1, maxlag=NULL, partial=FALSE, add=FALSE, lty=1,
	xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, main=NULL, ...){
	if(any(wt!=0&&wt!=1))stop("weights must be 0 or 1")
	len <- length(y)
	if(length(wt)==1)wt <- rep(1,len)
	if(any(is.na(y)))wt[is.na(y)] <- 0
	wt <- wt>0
	num <- sum(wt)
	limit <- 2/sqrt(num)
	if(is.null(maxlag))maxlag <- trunc(num/4)
	ave <- sum(y*wt,na.rm=TRUE)/num
	var <- sum((y-ave)^2*wt,na.rm=TRUE)/(num-1)
	co <- rep(1,maxlag+1)
	for(lag in 1:maxlag)
		co[lag+1] <- sum((y[1:(len-lag)]-ave)*(y[(lag+1):len]-ave)*
			wt[1:(len-lag)]*wt[(lag+1):len],na.rm=TRUE)/
			var/(sum(wt[1:(len-lag)]*wt[(lag+1):len],na.rm=TRUE)-1)
	if(partial){
		a <- tmp <- rep(0,maxlag)
		a[1] <- tmp[1] <- co[2]
		for(lag in 2:maxlag){
			a[lag] <- tmp[lag] <- (co[lag+1]-sum(a[1:(lag-1)]*
				co[lag:2]))/(1-sum(a[1:(lag-1)]*co[2:lag]))
			a[1:(lag-1)] <- a[1:(lag-1)]-a[lag]*a[(lag-1):1]}
		co <- tmp}
	start <- if(partial)1 else 0
	if(is.null(xlim))xlim <- c(0,maxlag)
	if(is.null(ylim))ylim <- c(if(min(co)<0)min(c(min(co),-limit))else 0,1)
	if(is.null(xlab))xlab <- "Lag"
	if(is.null(ylab))ylab <- expression(rho)
	if(is.null(main))main <- if(partial)"Partial Correlogram"
		else"Correlogram"
	if(add)lines(start:maxlag,co,lty=lty)
	else {
		plot(start:maxlag,co,type="l",lty=lty,xlim=xlim,ylim=ylim,
			xlab=xlab,ylab=ylab,main=main,...)
		abline(h=limit,lty=3)
		if(min(co)<0)abline(h=-limit,lty=3)
	abline(h=0)}
	invisible(cbind(start:maxlag,co))}
