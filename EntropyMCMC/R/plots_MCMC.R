# various functions for plotting objects of the MCMCEntropy package
# 3D plot of a formal function zft(z,z,...) 
# on mesh [l,r]^2
plottarget3d <- function(zft, l=-1, r=1, ms=10, theta=120, phi=20, ...){
	z <- seq(l,r,len=ms) # mesh size
	zz <- outer(z,z,function(u,v) zft(cbind(u,v),...))
	persp(z,z,zz,theta=theta,phi=phi,shade=0.5,
			xlab="x",ylab="y",
			ticktype="detailed", col="lightblue",border=NA)
	}

## plot method for plMCMC object
## x = plMCMC object, eventually a single chain when nmc=1
## xax, yax = x and y- axis coordinates for 2D projection plot
## ... = further parameters EXCEPT pch, passed to plot
## limited to a 2D projection path
plot.plMCMC <- function(x, xax=1, yax=2, title=NULL, cname=NULL, ...){
	n <- dim(x$Ptheta)[1]
	d <- dim(x$Ptheta)[2]
	nmc <- dim(x$Ptheta)[3]
	alltheta <- CollectChains(x)
	if (nmc == 1) tt <- paste(x$algo,
			" projection on axis (",xax, ",", yax,"), single chain", sep="")
			else tt <- paste(x$algo,
				" projection on axis (",xax, ",", yax,"), ",nmc," chains", sep="")
	if (!is.null(title)) tt <- title
	coord <- c(xax,yax)
	if (is.null(cname)) cname <- "var"
	plot(alltheta[,coord], pch=20, cex=0.2, main=tt,
		xlab=paste(cname,coord[1]), ylab=paste(cname,coord[2]), ...)
	}
	
	
## plot method for KbMCMC objects returned by EntropyMCMC() or EntropyParallel()
# class "KbMCMC" stores Kullback and Entropy estimates and misc infos
# lim=  zooming over 1:lim iterations only
# Kullback=	TRUE for plotting Kullback K(p^n,f), 
#			FALSE for plotting Entropy E_{p^n}(log p^n)
# title default title built from Kb object
# ... further parameters passed to plot or lines
plot.KbMCMC <- function(x, Kullback=TRUE, lim=NULL, ylim=NULL, 
                        new.plot=TRUE, title=NULL, ...){
		if (is.null(lim)) lim <- length(x$Kullback)
#		if (is.null(ylim)) ylim <- c(0,max(Kb$Kullback))
		if (is.null(title)) {
			tt <- paste("Algorithm: ", x$algo,", d=", x$dim,", ",
			            x$nmc," chains, k=", x$k, sep="")}
		else tt <- title
		if (Kullback) {
			y <- x$Kullback[1:lim]; ytt = "Kullback div. to target"
			} else {
				y <- x$Entp[1:lim]; ytt = "Entropy"
				}				
		if (new.plot) {
			plot(y, type="l", ylim=ylim, 
				xlab="iteration", ylab=ytt, main=tt, ...)
			}
			else lines(y, ...)	
		if (Kullback) abline(0,0,lty=2) 
	}
	
#############################################
## plot a list of KbMCMC objects returned by EntropyMCMC()
# which requests several types of graphics
# which=1 for comparison of different MCMC using same nmc
#      =2 for comparison among different nb of parallel chains
# 
# Preliminary version: assumes HM MCMC are used so that
# a q_param$v object is defined
# lim=  zooming over 1:lim iterations (time) only
# ylim if user-defined limits are needed

plot_Kblist <- function(Kb, which=1, lim=NULL, ylim=NULL){
	if (is.list(Kb)) { # argument is a list of KbMCMC objects !!pb T also if Kb
		# is a single KbMCMC object !!
		nk <- length(Kb) # nb of objects
		lg <- rep("",nk)
		if (which == 1) {
			for (i in 1:nk) lg[i] <- paste(Kb[[i]]$algo,
						", var=",Kb[[i]]$q_param$v[1,1], 
						", Method:",Kb[[i]]$method, ", k=", Kb[[i]]$k, sep="")
			tt <- paste("Kullback estimates, dim=", Kb[[1]]$dim,
					", ", Kb[[1]]$nmc, " chains", sep="")
					}
		if (which == 2) {
			for (i in 1:nk) lg[i] <- paste(Kb[[i]]$algo,
									", nmc =",Kb[[i]]$nmc, sep="") 
			tt <- paste("Kullback estimates, dim=",Kb[[i]]$dim, sep="")
					}
		if (is.null(lim)) lim <- length(Kb[[1]]$Kullback)
		if (is.null(ylim)) 
			ylim <- c(min(Kb[[1]]$Kullback),max(Kb[[1]]$Kullback))
		plot(Kb[[1]]$Kullback[1:lim],type="l",main=tt, col=1,
			ylim=ylim,
			xlab="iteration",ylab="Kullback")
		for (i in 2:nk) lines(Kb[[i]]$Kullback[1:lim],col=i)
		abline(0,0,lty=2) # horizontal line
		legend("topright", legend=lg, col=1:nk, lty=1, lwd=1)
		}
	else plot(Kb, lim=lim, title=tt, ylim) # assumes Kb = single KbMCMC object
	}
