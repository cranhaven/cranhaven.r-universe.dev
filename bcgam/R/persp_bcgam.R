#' @title 3D plots for \pkg{bcgam} fits 
#'
#' @description 3D plots of fitted surface with credible interval estimates based on a \code{bcgam} object and two non-parametrically
#' modelled predictors. 
#'
#' @param x Object of class inheriting from "bcgam".
#' @param x1 A non-parametrically modelled predictor in a bcgam fit. The predictor in the \eqn{x} axis. There is no default variable.
#' @param x2 A non-parametrically modelled predictor in a bcgam fit. The predictor in the \eqn{y} axis. There is no default variable.
#' @param parameter The type of parameter to be plotted. If parameter=\code{"eta"}, then the systematic component
#' \eqn{\eta} is plotted. If parameter=\code{"mu"}, then the mean value \eqn{\mu} obtained by transforming
#' \eqn{\eta} using the inverse of the link function is plotted. The default is \code{"mu"}. 
#' @param level Tolerance/credible level. The default is \code{0.95}.
#' @param x1.grid A positive integer that specifies how dense the \eqn{x} grid will be. The default is \code{10}.
#' @param x2.grid A positive integer that specifies how dense the \eqn{y} grid will be. The default is \code{10}.
#' @param lty What type of surface edges should be drawn for fitted values. The default is a solid line (\code{1}).
#' @param lty.inter What type of surface edges should be drawn for interval estimates. The default is a solid line (\code{1}).
#' @param ticktype character: "detailed" draws normal ticks; "simple" draws just an arrow parallel to the
#' axis to indicate direction of increase. The default is \code{"detailed"}.
#' @param col Color of fitted surface. The default is blue (\code{4}).
#' @param col.inter Color of interval estimates. If not specified, it takes the
#' same value as \code{col}.
#' @param surf.inter Indicator to draw interval estimates (\code{"TRUE"}) or not (\code{"FALSE"}). 
#' The default is \code{"TRUE"}.
#' @param zlim The \code{z} limits of the plot. If \code{zlim=NULL} and \code{surf.inter="TRUE"}, 
#' then the minimum value of the lower bound estimates and the maximum value of the upper bound estimates are used.
#' If \code{zlim=NULL} and \code{surf.inter="FALSE"}, then the minimum and maximum values of the fitted surface are used.
#' The default is \code{NULL}. 
#' @param ... additional graphical parameters.
#'
#' @export
#'
#' @import graphics
#'
#' @details \code{persp.bcgam} produces 3D plots based on the \code{bcgam} object. Interval
#' estimates are based on the specified \code{level}. 
#'
#' If there are more than two non-parametrically modelled predictors,
#' then these will be evaluated at the largest values that are smaller than or equal 
#' to their median values. Categorical covariates will be evaluated at their mode. Also,
#' continuous covariates will be evaluated at the largest values that are smaller than
#' or equal to their median values.
#'
#' This routine creates 3D plots based on the posterior distribution in the \code{bcgam} object.
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
#' persp(bcgam.fit, x1, x2, parameter="eta", col.inter=2, level=0.90, theta=-55)
#' }
persp.bcgam <- function(x, x1, x2, parameter="mu", level=0.95, x1.grid=10, x2.grid=10, 
			lty=1, lty.inter=1, ticktype="detailed", col=4, col.inter=col, surf.inter=TRUE, zlim=NULL, ...){
	if(missing(x1)|missing(x2)){stop("variable x not specified!")}

	else{
	tt <- terms(x)
    	if (!inherits(x, "bcgam")) 
      warning("calling persp.bcgam(<fake-bcgam-object>) ...")
      Terms <- delete.response(tt)
	
	mf<-match.call(expand.dots=FALSE)
	m <- match(c("x1","x2"), names(mf))
	mf<-mf[m]


	x1x2.position=match(c(as.character(mf)), colnames(x$default.plotvalues) )	

		if(sum(is.na(x1x2.position))>0 ){stop("variable x1 or x2 not in data frame!")}

		else{
		x1.var=as.vector(x$data[,x1x2.position[1]+1])
		x1.var.name=colnames(x$default.plotvalues[x1x2.position[1]])

		x2.var=as.vector(x$data[,x1x2.position[2]+1])
		x2.var.name=colnames(x$default.plotvalues[x1x2.position[2]])

		rng.x1=range(x1.var)
		seq.x1=seq(rng.x1[1],rng.x1[2],length.out=x1.grid)
		rng.x2=range(x2.var)
		seq.x2=seq(rng.x2[1],rng.x2[2],length.out=x2.grid)

		grid.x1x2=expand.grid(seq.x1,seq.x2)

		rep.plotvalues=x$default.plotvalues[rep(1,dim(grid.x1x2)[1]),]
		rep.plotvalues[,x1x2.position[1]]=grid.x1x2[,1]
		rep.plotvalues[,x1x2.position[2]]=grid.x1x2[,2]
		
		predtoplot=predict(x,rep.plotvalues, parameter=parameter, level=level)
		rgb.col=c(col2rgb(col))
		normal.col=rgb(rgb.col[1],rgb.col[2],rgb.col[3], maxColorValue=255)
		
		if(surf.inter==FALSE){
			if(is.null(zlim)){
				zlim[1]=min(predtoplot$cred.mean)
				zlim[2]=max(predtoplot$cred.mean)
			}
		persp(x=seq.x1, y=seq.x2, z=matrix(predtoplot$cred.mean, nrow=x1.grid), 
			zlim=zlim, col=normal.col, xlab=x1.var.name, ylab=x2.var.name, zlab=predtoplot$y.lab, ticktype=ticktype, ...)
		}
		
		else{ #surf.inter==TRUE
			if(is.null(zlim)){
				zlim[1]=min(predtoplot$cred.lower)
				zlim[2]=max(predtoplot$cred.upper)
			}
			else{zlim=zlim}
		persp(x=seq.x1, y=seq.x2, z=matrix(predtoplot$cred.mean, nrow=x1.grid), 
			zlim=zlim, col=normal.col, xlab=x1.var.name, ylab=x2.var.name, zlab=predtoplot$y.lab, ticktype=ticktype,
			lty=lty, ...)
		rgb.transp.col=c(col2rgb(col.inter))
		transp.col=rgb(rgb.transp.col[1],rgb.transp.col[2],rgb.transp.col[3], alpha=50, maxColorValue=255)
		par(new=TRUE)
		persp(x=seq.x1, y=seq.x2, z=matrix(predtoplot$cred.lower, nrow=x1.grid), 
			xlab=x1.var.name, ylab=x2.var.name, zlab=predtoplot$y.lab, ticktype=ticktype,
			zlim=zlim, col=transp.col, lty=lty.inter, ...)
		par(new=TRUE)
		persp(x=seq.x1, y=seq.x2, z=matrix(predtoplot$cred.upper, nrow=x1.grid), 
			xlab=x1.var.name, ylab=x2.var.name, zlab=predtoplot$y.lab, ticktype=ticktype,
			zlim=zlim, col=transp.col, lty=lty.inter, ...)
		} #end surf.inter
		}
	}
}
