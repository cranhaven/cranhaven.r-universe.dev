#' @title 2D plots for \pkg{bcgam} fits 
#'
#' @description 2D plots of predicted values with interval estimates based on a \code{bcgam} object and one non-parametrically
#' modelled predictor. 
#'
#' @param x Object of class inheriting from "bcgam".
#' @param x1 A non-parametrically modelled predictor in a bcgam fit. There is no default variable.
#' @param interval Type of interval to plot. It can be either \code{"credible"} or \code{"prediction"}.
#' \code{interval="prediction"} only works for the gaussian family. In this case, the predictive posterior distribution is plotted. 
#' The default is \code{"credible"}.
#' @param parameter The type of parameter to be plotted. If parameter=\code{"eta"}, then the systematic component
#' \eqn{\eta} is plotted. If parameter=\code{"mu"}, then the mean value \eqn{\mu} obtained by transforming
#' \eqn{\eta} using the inverse of the link function is plotted. The default is \code{"mu"}. 
#' If \code{interval="prediction"}, then this variable is ignored.
#' @param x1.grid A positive integer that specifies how dense the \eqn{x} grid will be. The default is \code{100}.
#' @param level Tolerance/credible level. The default is \code{0.95}.
#' @param type What type of plot should be drawn. The default is for lines (\code{"l"}). 
#' @param col Color of fitted values. The default is black (\code{1}).
#' @param col.inter Color of interval estimates. If not specified, it takes the
#' same value as \code{col}.
#' @param lty What type of line should be drawn for fitted values. Ignored when a line is not being drawn. 
#' The default is a solid line (\code{1}).
#' @param lty.inter What type of line should be drawn for interval estimates. The default is a dotted line (\code{2}).
#' @param lwd The line width of fitted values, a positive number, defaulting to \code{1}.
#' @param lwd.inter The line width of interval estimates, a positive number, defaulting to \code{1}.
#' @param ylim The \code{y} limits of the plot. If \code{ylim=NULL}, then the minimum value
#' of the lower bound estimates and the maximum value of the upper bound estimates are used.
#' The default is \code{NULL}. 
#' @param ... other parameters to be passed through to plotting functions.
#'
#' @export
#'
#' @import graphics 
#'
#' @details \code{plot.bcgam} produces 2D plots based on the \code{bcgam} object. Interval
#' estimates are based on the specified \code{level}. 
#'
#' If there are more than one non-parametrically modelled predictors,
#' then these will be evaluated at the largest values that are smaller than or equal 
#' to their median values. Categorical covariates will be evaluated at their mode. Also,
#' continuous covariates will be evaluated at the largest values that are smaller than
#' or equal to their median values.
#'
#' This routine creates 2D plots based on the posterior distribution in the \code{bcgam} object.
#'
#' @author Cristian Oliva-Aviles and Mary C. Meyer
#'
#' @keywords internal
#' 
#' @examples
#' \dontrun{
#' n<-50
#' x1<-(1:n/n)^{1/3}
#' x2<-log(1:n/n)
#' z<-as.factor(rbinom(n, 1, 0.6))
#' eta<-x1+x2+0.2*as.numeric(z)+rnorm(n, sd=0.2)
#' mu<-exp(eta)/(1+exp(eta))
#' y<-(mu<0.6)
#'
#' bcgam.fit <- bcgam(y~sm.incr(x1)+sm.conc(x2, numknots=8)+z, nloop=10000, family="binomial")
#' plot(bcgam.fit, x1, parameter="mu", col=4, level=0.90)
#' plot(bcgam.fit, x2, parameter="eta", col=3, col.inter=2)
#' }
plot.bcgam <- function(x, x1, interval="credible", parameter="mu", x1.grid=100, level=0.95, type="l", col=1,
			col.inter=col, lty=1, lty.inter=2, lwd=1, lwd.inter=lwd, ylim=NULL, ...)
{
	if(missing(x1)){stop("variable x1 not specified!")}

	else{
	tt <- terms(x)
    	if (!inherits(x, "bcgam")) 
      warning("calling plot.bcgam(<fake-bcgam-object>) ...")
      Terms <- delete.response(tt)
	
	mf<-match.call(expand.dots=FALSE)
	m <- match(c("x1"), names(mf))
	mf<-mf[m]


	x1.position=match(c(as.character(mf)), colnames(x$default.plotvalues) )	
		
		if(is.na(x1.position)){stop("variable x1 not in data frame!")}
		else{
		x1.var=as.vector(x$data[,x1.position+1])
		x1.var.name=colnames(x$default.plotvalues[x1.position])

		rng.x1=range(x1.var)
		seq.x1=seq(rng.x1[1],rng.x1[2],length.out=x1.grid)
		rep.plotvalues=x$default.plotvalues[rep(1,length(seq.x1)),]
		rep.plotvalues[,x1.position]=seq.x1

	if(interval=="credible"){
		predtoplot=predict(x,rep.plotvalues, interval="credible", parameter=parameter,level=level)
		if(is.null(ylim)){
			ylim[1]=min(predtoplot$cred.lower)				
			ylim[2]=max(predtoplot$cred.upper)				
		}
		else{ylim=ylim}

		plot(seq.x1,predtoplot$cred.mean, xlab=x1.var.name, ylab=predtoplot$y.lab, ylim=ylim, type=type, col=col, lty=lty, lwd=lwd, ...)
		lines(seq.x1,predtoplot$cred.lower, col=col.inter, lty=lty.inter, lwd=lwd.inter,...)
		lines(seq.x1,predtoplot$cred.upper, col=col.inter, lty=lty.inter, lwd=lwd.inter,...)
	}

	if(interval=="prediction" & x$family=="gaussian"){
		predtoplot=predict(x,rep.plotvalues, interval="prediction", parameter=parameter,level=level)
		if(is.null(ylim)){
			ylim[1]=min(predtoplot$pred.lower)				
			ylim[2]=max(predtoplot$pred.upper)				
		}
		else{ylim=ylim}

		plot(seq.x1,predtoplot$pred.mean, xlab=x1.var.name, ylab=predtoplot$y.lab, ylim=ylim, type=type, col=col, lty=lty, lwd=lwd, ...)
		lines(seq.x1,predtoplot$pred.lower, col=col.inter, lty=lty.inter, lwd=lwd.inter,...)
		lines(seq.x1,predtoplot$pred.upper, col=col.inter, lty=lty.inter, lwd=lwd.inter,...)
	}	

	if(interval=="prediction" & x$family=="binomial"){
		stop("interval not valid with binomial family")}
	if(interval=="prediction" & x$family=="poisson"){
		stop("interval not valid with poisson family")}

		}
	}
}
