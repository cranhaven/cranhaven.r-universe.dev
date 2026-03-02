#######################################################
#	apc package
#	Bent Nielsen, 17 Jan 2024, version 3.0.0
#	functions to test for 2 sample analysis
#######################################################
#	Copyright 2023-2023 Bent Nielsen
#	Nuffield College, OX1 1NF, UK	
#	bent.nielsen@nuffield.ox.ac.uk
#
#	This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#######################################################

###########################################
#	plot parameters
###########################################

apc.plot.fit.2s	<- function(apc.fit.model.1,apc.fit.model.2,type="macro",
                which.plot=0,
								col=NULL,label=NULL,lty=NULL
								)
#								scale=FALSE,sdv.at.zero=TRUE,type="detrend",
#								include.linear.plane=TRUE,include.double.differences=TRUE,
#								sub.plot=NULL,main.outer=NULL,main.sub=NULL,
#								cex=NULL,cex.axis=NULL,cex.lab=NULL,cex.main=NULL,
#								cex.main.outer=1.2,line.main=0.5,line.main.outer=NULL,
#								las=NULL,mar=NULL,oma=NULL,mgp=c(2,1,0),
#								vec.xlab=NULL)
{	#	apc.2s.plot.fit
	#################
	#	get input
	unit	<- apc.fit.model.1$unit
	#################
	#	identify
	if(unit[2]!=unit[3])	{	# mixed samples
		id.1	<- apc.identify.mixed(apc.fit.model.1)
		id.2	<- apc.identify.mixed(apc.fit.model.2)
		if(is.null(label))	{label	<- c(id.1$label,id.2$label)}
	}
	#################
	#	get identified variables
	G		<- id.1$G
	H		<- id.1$H
	#################	
	#################	
	#################	
	#	Function to plot two parameter fit curves in one.
	plot.dif.fit<-	function(local.fit.1,local.fit.2,local.col=c("black","blue"),local.lty=c(1,3)    ,local.pch=c(19,19)  ,main=NULL)
	#	Takes two fits that are separately estimated so that their covariance is zero.
	#	Plots the difference
	{	#	plot.1s.fit
		y.1		<- local.fit.1[,1]
		y.2		<- local.fit.2[,1]
		s.1		<- local.fit.1[,2]
		s.2		<- local.fit.2[,2]
		x.1		<- local.fit.1[,3]
		x.2		<- local.fit.2[,3]
		y.dif	<- y.1 - y.2
		s.dif	<- sqrt(s.1^2 + s.2^2)
		ylim	<- c(min(y.dif,-s.dif*2,na.rm=TRUE),max(y.dif,s.dif*2,na.rm=TRUE))
		xlim	<- c(min(x.1),max(x.1))
		  plot(NULL,xaxt="n",xlab="",ylab="",xlim=xlim,ylim=ylim,main=main)
	 	  axis(side=1,at=x.1)
		 lines(x.1, y.dif  ,col=local.col[1],lty=local.lty[1])
		points(x.1, y.dif  ,col=local.col[1],pch=local.pch[1])
		 lines(x.1,-s.dif*2,col=local.col[1],lty=local.lty[2])
		 lines(x.1, s.dif*2,col=local.col[1],lty=local.lty[2])
		points(x.1,-s.dif*2,col=local.col[1],pch=local.pch[2],cex=0.6)
		points(x.1, s.dif*2,col=local.col[1],pch=local.pch[2],cex=0.6)
	}	#	plot.1s.fit
	#################	
	#################	
	#################	
	#	Function to plot two parameter fit curves in one.	
	plot.2s.fit	<-	function(local.fit.1,local.fit.2,local.col=c("black","blue"),local.lty=c(1,3,1,3),local.pch=c(19,19,1,1),main=NULL,local.label=c("sample 1","sample 2"))
	#	Takes two fits that are separately estimated so that their covariance is zero.
	#	Plots each fit
	{	#	plot.2s.fit
		y.1		<- local.fit.1[,1]
		y.2		<- local.fit.2[,1]
		s.1		<- local.fit.1[,2]
		s.2		<- local.fit.2[,2]
		x.1		<- local.fit.1[,3]
		x.2		<- local.fit.2[,3]
		ylim	<- c(min(y.1,y.2,-s.1*2,-s.2*2,na.rm=TRUE),max(y.1,y.2,s.1*2,s.2*2,na.rm=TRUE))
		xlim	<- c(min(x.1),max(x.1))
	  	  plot(NULL,xaxt="n",xlab="",ylab="",xlim=xlim,ylim=ylim,main=main)
		  axis(side=1,at=x.1);
		legend("topright",col=local.col,lty=local.lty[c(1,3)],pch=local.pch[c(1,3)],legend=local.label)
		 lines(x.1, y.1,col=local.col[1],lty=local.lty[1])
		 lines(x.2, y.2,col=local.col[2],lty=local.lty[3])
		points(x.1, y.1,col=local.col[1],pch=local.pch[1])		  
		points(x.2, y.2,col=local.col[2],pch=local.pch[3])
		 lines(x.1,-s.1*2,col=local.col[1],lty=local.lty[2])
		 lines(x.1, s.1*2,col=local.col[1],lty=local.lty[2])
		 lines(x.2, s.2*2,col=local.col[2],lty=local.lty[4])
		 lines(x.2,-s.2*2,col=local.col[2],lty=local.lty[4])
		points(x.1,-s.1*2,col=local.col[1],pch=local.pch[2],cex=0.6)
		points(x.1, s.1*2,col=local.col[1],pch=local.pch[2],cex=0.6)
		points(x.2,-s.2*2,col=local.col[2],pch=local.pch[4],cex=0.6)
		points(x.2, s.2*2,col=local.col[2],pch=local.pch[4],cex=0.6)
	}	#	plot.2s.fit
	#################	
	# main code	
	#################	
	#	Detrend plots
	if(type=="micro")	{
	  wp          <- which.plot
		main.alpha	<- "Demeaned age micro effect"
		main.beta	  <- "Demeaned period micro effect"
		main.gamma	<- "Demeaned cohort micro effect"
		if(wp==0) dev.new();
		if(H>1   && wp%in%c(0,1)){plot.2s.fit(id.1$coefficients.micro.dem.stack.alpha,id.2$coefficients.micro.dem.stack.alpha,main=main.alpha,local.label=label)	}	
		if(wp==0) dev.new(); 
		if(G>1   && wp%in%c(0,2)){plot.2s.fit(id.1$coefficients.micro.dem.stack.beta ,id.2$coefficients.micro.dem.stack.beta ,main=main.beta ,local.label=label)	}	
		if(wp==0) dev.new(); 
		if(G*H>1 && wp%in%c(0,3)){plot.2s.fit(id.1$coefficients.micro.dem.stack.gamma,id.2$coefficients.micro.dem.stack.gamma,main=main.gamma,local.label=label)	}	
	  main.alpha	<- "Demeaned age micro difference"
		main.beta	  <- "Demeaned period micro difference"
		main.gamma	<- "Demeaned cohort micro difference"
		if(wp==0) dev.new(); 
		if(H>1   && wp%in%c(0,4)){plot.dif.fit(id.1$coefficients.micro.dem.stack.alpha,id.2$coefficients.micro.dem.stack.alpha,main=main.alpha)	}	
		if(wp==0) dev.new(); 
		if(G>1   && wp%in%c(0,5)){plot.dif.fit(id.1$coefficients.micro.dem.stack.beta ,id.2$coefficients.micro.dem.stack.beta ,main=main.beta )	}	
		if(wp==0) dev.new(); 
		if(G*H>1 && wp%in%c(0,6)){plot.dif.fit(id.1$coefficients.micro.dem.stack.gamma,id.2$coefficients.micro.dem.stack.gamma,main=main.gamma)	}	
	}	
	#	double sum of double difference macro plots
	if(type=="macro.ssdd")	{
		warning(paste("Show double sum of double differences for macro effect +- 2 s.e. \n",
					  "The ss.dd cohort plot may show large s.e. as parameters for early cohorts poorly determined."))
	  wp          <- which.plot
		main.alpha	<- "Double sum age macro effect"
		main.beta	  <- "Double sum period macro effect"
		main.gamma	<- "Double sum cohort macro effect"
		if(wp==0) dev.new(); 
		if(wp%in%c(0,1)) plot.2s.fit(id.1$coefficients.macro.ssdd.alpha[[1]],id.2$coefficients.macro.ssdd.alpha[[1]],local.label=label,main=main.alpha)
		if(wp==0) dev.new(); 
		if(wp%in%c(0,2)) plot.2s.fit(id.1$coefficients.macro.ssdd.beta[[1]] ,id.2$coefficients.macro.ssdd.beta[[1]] ,local.label=label,main=main.beta	)
		if(wp==0) dev.new(); 
		if(wp%in%c(0,3)) plot.2s.fit(id.1$coefficients.macro.ssdd.gamma[[1]],id.2$coefficients.macro.ssdd.gamma[[1]],local.label=label,main=main.gamma)
		main.alpha	<- "Double sum age macro difference"
		main.beta	  <- "Double sum period macro difference"
		main.gamma	<- "Double sum cohort macro difference"
		if(wp==0) dev.new(); 
		if(wp%in%c(0,4)) plot.dif.fit(id.1$coefficients.macro.ssdd.alpha[[1]],id.2$coefficients.macro.ssdd.alpha[[1]],main=main.alpha)
		if(wp==0) dev.new(); 
		if(wp%in%c(0,5)) plot.dif.fit(id.1$coefficients.macro.ssdd.beta[[1]] ,id.2$coefficients.macro.ssdd.beta[[1]] ,main=main.beta	)
		if(wp==0) dev.new(); 
		if(wp%in%c(0,6)) plot.dif.fit(id.1$coefficients.macro.ssdd.gamma[[1]],id.2$coefficients.macro.ssdd.gamma[[1]],main=main.gamma)
	}
	#	detrended macro plots
	if(type %in% c("macro","macro-detrend"))	{
		  warning("show detrended macro effect +- 2 s.e.")
  	  wp          <- which.plot
	  	main.alpha	<- "Detrended age macro effect"
		  main.beta	<- "Detrended period macro effect"
		  main.gamma	<- "Detrended cohort macro effect"
      if(wp==0) dev.new(); 
		  if(wp%in%c(0,1)) plot.2s.fit(id.1$coefficients.macro.det.alpha[[1]],id.2$coefficients.macro.det.alpha[[1]],main=main.alpha,local.label=label)
      if(wp==0) dev.new(); 
		  if(wp%in%c(0,2)) plot.2s.fit(id.1$coefficients.macro.det.beta[[1]] ,id.2$coefficients.macro.det.beta[[1]] ,main=main.beta	,local.label=label)
      if(wp==0) dev.new(); 
		  if(wp%in%c(0,3)) plot.2s.fit(id.1$coefficients.macro.det.gamma[[1]],id.2$coefficients.macro.det.gamma[[1]],main=main.gamma,local.label=label)
  		main.alpha	<- "Detrended age macro difference effect"
	  	main.beta	<- "Detrended period macro difference effect"
		  main.gamma	<- "Detrended cohort macro difference effect"
      if(wp==0) dev.new(); 
		  if(wp%in%c(0,4))  plot.dif.fit(id.1$coefficients.macro.det.alpha[[1]],id.2$coefficients.macro.det.alpha[[1]],main=main.alpha)
      if(wp==0) dev.new(); 
		  if(wp%in%c(0,5))  plot.dif.fit(id.1$coefficients.macro.det.beta[[1]] ,id.2$coefficients.macro.det.beta[[1]] ,main=main.beta	)
      if(wp==0) dev.new(); 
		  if(wp%in%c(0,6))  plot.dif.fit(id.1$coefficients.macro.det.gamma[[1]],id.2$coefficients.macro.det.gamma[[1]],main=main.gamma)
	}
}	#	apc.2s.plot.fit

