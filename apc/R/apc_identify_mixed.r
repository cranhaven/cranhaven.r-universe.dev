#######################################################
#	apc package
#	Bent Nielsen, 15 December 2023, version 3.0.0
#	functions to identify parameters in two sample analysis
#######################################################
#	Copyright 2023 Bent Nielsen
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

#########################################################
#	apc.identify.mixed
#########################################################
apc.identify.mixed	<- function(apc.fit.model)
{	#	apc.identify.mixed
#	##############################
#	#	check input		VERY SLOW IS THERE ANOTHER WAY?
#	if(isFALSE("model.design.common" %in% apc.fit.model))
#		warning("is the argument a 2 sample fit?")
	##############################
	#	get values
	coefficients		<- apc.fit.model$coefficients.canonical		#	4 columns
	covariance			<- apc.fit.model$covariance.canonical
    index.age		    <- apc.fit.model$index.age
    index.per		    <- apc.fit.model$index.per
    index.coh		    <- apc.fit.model$index.coh
	index.trap			<- apc.fit.model$index.trap
    model.design		<- apc.fit.model$model.design
	age1				<- apc.fit.model$age1
	per1				<- apc.fit.model$per1
	coh1				<- apc.fit.model$coh1
	age.max				<- apc.fit.model$age.max
	per.max				<- apc.fit.model$per.max
	coh.max				<- apc.fit.model$coh.max
	unit				<- apc.fit.model$unit	
	data.format			<- apc.fit.model$data.format
	dates				<- apc.fit.model$dates
	I.coh				<- apc.fit.model$I.coh
	label				<- apc.fit.model$label
	G					<- apc.fit.model$G		
	H					<- apc.fit.model$H		
####	v.g					<- apc.fit.model$v.g	
####	v.h					<- apc.fit.model$v.h	
####	v.q.g				<- apc.fit.model$v.q.g
####	v.r.g				<- apc.fit.model$v.r.g
####	v.q.h				<- apc.fit.model$v.q.h
####	v.r.h				<- apc.fit.model$v.r.h
	##############################
	#	Get Euclidean representation
	G	<- unit[2];
	H	<- unit[3];
	v.g	<- age.max-index.trap[,1]		#	g, so that age=A-g*G
	v.h	<- index.trap[,2]-1				#	h, so that per=H+h*H     
	v.g.q	<- v.g %/% H						#	Euclidean representation	
	v.g.r	<- v.g %%  H						#   g = v.g*G+r.g            
	v.h.q	<- v.h %/% G                                                   
	v.h.r	<- v.h %%  G
	v.gh	<- H + v.g*G + v.h*H
	##############################
	#	get macro indices
	index.alpha.macro	<- intersect(which(v.h==0)  ,which(v.g.r==0))		# selects rows where h=per/H-1=0 	&& g.r==0
	index.beta.macro	<- intersect(which(v.g==0)  ,which(v.h.r==0))		# selects rows where g=(A-age)/G=0  && h.r==0
	set					<- union(which(v.g==0),which(v.h>per.max-G-1))
	set					<- intersect(set,which(v.g.r==0))
	set					<- intersect(set,which(v.h.r==0))
	order				<- order(v.gh[set])
	index.gamma.macro	<- set[order]
	##############################
	#	get all indices
	index.alpha	<- index.beta	<- index.gamma	<- NULL
	for(i in 1:H)	{
		v	<- intersect(which(v.h==0)  ,which(v.g.r==i-1))	# selects rows where h=per/H-1=0 	&& g.r==i
		index.alpha	<- c(index.alpha,list(v))
	}
	for(j in 1:G)	{
		v	<- intersect(which(v.g==0)  ,which(v.h.r==j-1))	# selects rows where g=(A-age)/G=0  && h.r==i
		index.beta	<- c(index.beta,list(v))
	}
	for(i in 1:H)	{
		for(j in 1:G)	{
			set		<- union(which(v.g<H),which(v.h>per.max-G-1))	# selects last H columns and last G rows
			set		<- intersect(set,which(v.g.r==i-1))	# selects subset where g.r==i
			set		<- intersect(set,which(v.h.r==j-1))	# selects subset where h.r==j
			order	<- order(v.gh[set])
			v		<- set[order]
			index.gamma	<- c(index.gamma,list(v))
		}
	}
	##############################
	#	get design matrix
	m.design		<- apc.get.design(apc.fit.model)$design
	##############################
	#	get dates
	v.dates.long.age	<- age1	+ unit[1]*G*(0:age.max)
	v.dates.long.per	<- per1	+ unit[1]*H*(0:per.max)
	v.dates.long.coh	<- coh1 + unit[1]*(0:(age.max*G+per.max*H))
	##############################
	#	get detrended macro coefficients
	##############################
	#	get alpha macro detrend
	i			<- 1
	index.para	<- index.alpha[[i]]
	index.time	<- index.age
	v.dates		<- v.dates.long.age[(1:length(index.para)-1)*H+i]
	m.X			<- apc.internal.function.detrend(length(index.para)) 
	m.X			<- m.X %*% m.design[index.para,index.time]
	v.coef		<- m.X %*% coefficients[index.time,1]
	v.cov		<- m.X %*% covariance[index.time,index.time] %*% t(m.X)
	v.sdv		<- sqrt(diag(v.cov))
	m.coef		<- cbind(v.coef,v.sdv,v.dates)
	rownames(m.coef)<- m.coef[,3]
	colnames(m.coef)<- c("coefficient","sdv","date")				
	coefficients.macro.det.alpha	<- list(m.coef)
	##############################
	#	get beta macro detrend
	j			<- 1
	index.para	<- index.beta[[j]]
	index.time	<- index.per
	v.dates		<- v.dates.long.per[(1:length(index.para)-1)*G+j]
	m.X			<- apc.internal.function.detrend(length(index.para)) 
	m.X			<- m.X %*% m.design[index.para,index.time]
	v.coef		<- m.X %*% coefficients[index.time,1]
	v.cov		<- m.X %*% covariance[index.time,index.time] %*% t(m.X)
	v.sdv		<- sqrt(diag(v.cov))
	m.coef		<- cbind(v.coef,v.sdv,v.dates)
	rownames(m.coef)<- m.coef[,3]
	colnames(m.coef)<- c("coefficient","sdv","date")				
	coefficients.macro.det.beta	<- list(m.coef)
	##############################
	#	get gamma macro detrend
	ij			<- 1
	index.para	<- index.gamma[[ij]]
	index.time	<- index.coh
	v.dates		<- v.dates.long.coh[(1:length(index.para)-1)*G*H+ij]
	m.X			<- apc.internal.function.detrend(length(index.para)) 
	m.X			<- m.X %*% m.design[index.para,index.time]
	v.coef		<- m.X %*% coefficients[index.time,1]
	v.cov		<- m.X %*% covariance[index.time,index.time] %*% t(m.X)
	v.sdv		<- sqrt(diag(v.cov))
	m.coef		<- cbind(v.coef,v.sdv,v.dates)
	rownames(m.coef)<- m.coef[,3]
	colnames(m.coef)<- c("coefficient","sdv","date")				
	coefficients.macro.det.gamma	<- list(m.coef)	
	##############################
	#	get cummulated coefficients
	##############################
	#	get all alpha cummulated
	coefficients.macro.ssdd.alpha	<- NULL
	for(i in 1:H)	{
		index.para	<- index.alpha[[i]]
		index.time	<- index.age
		v.dates		<- v.dates.long.age[(1:length(index.para)-1)*H+i]
		m.X			<- m.design[index.para,index.time]
		v.coef		<- m.X %*% coefficients[index.time,1]
		v.cov		<- m.X %*% covariance[index.time,index.time] %*% t(m.X)
		v.sdv		<- sqrt(diag(v.cov))
		m.coef		<- cbind(v.coef,v.sdv,v.dates)
		rownames(m.coef)<- m.coef[,3]
		colnames(m.coef)<- c("coefficient","sdv","date")				
		coefficients.macro.ssdd.alpha	<- c(coefficients.macro.ssdd.alpha,list(m.coef))
   	}
	##############################
	#	get all beta cummulated
	coefficients.macro.ssdd.beta	<- NULL
	for(j in 1:G)	{
		index.para	<- index.beta[[j]]
		index.time	<- index.per
		v.dates		<- v.dates.long.per[(1:length(index.para)-1)*G+j]
		m.X			<- m.design[index.para,index.time]
		v.coef		<- m.X %*% coefficients[index.time,1]
		v.cov		<- m.X %*% covariance[index.time,index.time] %*% t(m.X)
		v.sdv		<- sqrt(diag(v.cov))
		m.coef		<- cbind(v.coef,v.sdv,v.dates)
		rownames(m.coef)<- m.coef[,3]
		colnames(m.coef)<- c("coefficient","sdv","date")				
		coefficients.macro.ssdd.beta	<- c(coefficients.macro.ssdd.beta,list(m.coef))
	}	
	##############################
	#	get all gamma cummulated
	coefficients.macro.ssdd.gamma	<- NULL
	for(i in 1:H)	{
		for(j in 1:G)	{
			ij			<- (i-1)*G+(j-1)+1
			index.para	<- index.gamma[[ij]]
			index.time	<- index.coh
			v.dates		<- v.dates.long.coh[(1:length(index.para)-1)*G*H+ij]
			m.X			<- m.design[index.para,index.time]
			v.coef		<- m.X %*% coefficients[index.time,1]
			v.cov		<- m.X %*% covariance[index.time,index.time] %*% t(m.X)
			v.sdv		<- sqrt(diag(v.cov))
			m.coef		<- cbind(v.coef,v.sdv,v.dates)
			rownames(m.coef)<- m.coef[,3]
			colnames(m.coef)<- c("coefficient","sdv","date")				
			coefficients.macro.ssdd.gamma	<- c(coefficients.macro.ssdd.gamma,list(m.coef))
		}
	}				
	##############################
	#	get differences at different micro frequences of
	#	cummulated coefficients
	##############################
	#	get alpha differences
	coefficients.micro.dem.alpha	<- NULL
	index.para.0<- index.alpha[[1]]
	index.time	<- index.age
	m.X.0			<- m.design[index.para.0,index.time]
	for(i in 1:H)	{
		index.para	<- index.alpha[[i]]
		v.dates		<- v.dates.long.age[(1:length(index.para)-1)*H+i]
		m.X			<- m.design[index.para,index.time]
		m.X			<- m.X - m.X.0[1:length(index.para),]
		v.coef		<- m.X %*% coefficients[index.time,1]
		v.cov		<- m.X %*% covariance[index.time,index.time] %*% t(m.X)
		v.sdv		<- sqrt(diag(v.cov))
		m.coef		<- cbind(v.coef,v.sdv,v.dates)
		rownames(m.coef)<- m.coef[,3]
		colnames(m.coef)<- c("coefficient","sdv","date")				
		coefficients.micro.dem.alpha	<- c(coefficients.micro.dem.alpha,list(m.coef))
	}	
	##############################
	#	get beta differences
	coefficients.micro.dem.beta	<- NULL
	index.para.0	<- index.beta[[1]]
	index.time		<- index.per
	m.X.0			<- m.design[index.para.0,index.time]
	for(j in 1:G)	{
		index.para	<- index.beta[[j]]
		v.dates		<- v.dates.long.per[(1:length(index.para)-1)*G+j]
		m.X			<- m.design[index.para,index.time]
		m.X			<- m.X - m.X.0[1:length(index.para),]
		v.coef		<- m.X %*% coefficients[index.time,1]
		v.cov		<- m.X %*% covariance[index.time,index.time] %*% t(m.X)
		v.sdv		<- sqrt(diag(v.cov))
		m.coef		<- cbind(v.coef,v.sdv,v.dates)
		rownames(m.coef)<- m.coef[,3]
		colnames(m.coef)<- c("coefficient","sdv","date")				
		coefficients.micro.dem.beta	<- c(coefficients.micro.dem.beta,list(m.coef))
	}	
	##############################new
	#	get gamma differences
	coefficients.micro.dem.gamma	<- NULL
	index.para.0<- index.gamma[[1]]
	index.time	<- index.coh
	m.X.0			<- m.design[index.para.0,index.time]
	for(i in 1:H)	{
		for(j in 1:G)	{
			ij			<- (i-1)*G+(j-1)+1
			index.para	<- index.gamma[[ij]]
			v.dates		<- v.dates.long.coh[(1:length(index.para)-1)*G*H+ij]
			m.X			<- m.design[index.para,index.time]
			m.X			<- m.X - m.X.0[1:length(index.para),]
			v.coef		<- m.X %*% coefficients[index.time,1]
			v.cov		<- m.X %*% covariance[index.time,index.time] %*% t(m.X)
			v.sdv		<- sqrt(diag(v.cov))
			m.coef		<- cbind(v.coef,v.sdv,v.dates)
			rownames(m.coef)<- m.coef[,3]
			colnames(m.coef)<- c("coefficient","sdv","date")				
			coefficients.micro.dem.gamma	<- c(coefficients.micro.dem.gamma,list(m.coef))
		}	
	}	
	function.combine.micro.series	<- function(local.fit.list)
	{	#	function.combine.micro.series
		len	<- length(local.fit.list)
		x	<- NULL
		for(l in 1:len){
			x	<- c(x,local.fit.list[[l]][,3])
		}
		s	<- y	<- rep(NA,nrow(local.fit.list[[1]]))
		if(len>1)
		for(l in 2:len){
			y	<- c(y,local.fit.list[[l]][,1])
			s	<- c(s,local.fit.list[[l]][,2])
		}
		x.order	<- order(x)
		x		<- x[x.order]
		y		<- y[x.order]
		s		<- s[x.order]
		m.coef	<- cbind(y,s,x)
		colnames(m.coef)<- c("coefficient","sdv","date")				
		return(m.coef)
	}	#	function.combine.micro.series

	coefficients.micro.dem.stack.alpha	<- function.combine.micro.series(coefficients.micro.dem.alpha)
	coefficients.micro.dem.stack.beta	<- function.combine.micro.series(coefficients.micro.dem.beta )
	coefficients.micro.dem.stack.gamma	<- function.combine.micro.series(coefficients.micro.dem.gamma)
	##############################										 
	return(list(
		G									=G 									,
		H									=H									,
		coefficients.macro.det.alpha		=coefficients.macro.det.alpha		,
		coefficients.macro.det.beta			=coefficients.macro.det.beta 		,
		coefficients.macro.det.gamma		=coefficients.macro.det.gamma		,
		coefficients.macro.ssdd.alpha		=coefficients.macro.ssdd.alpha		,
		coefficients.macro.ssdd.beta		=coefficients.macro.ssdd.beta 		,
		coefficients.macro.ssdd.gamma		=coefficients.macro.ssdd.gamma		,
		coefficients.micro.dem.alpha		=coefficients.micro.dem.alpha		,
		coefficients.micro.dem.beta			=coefficients.micro.dem.beta 		,
		coefficients.micro.dem.gamma		=coefficients.micro.dem.gamma		,
		coefficients.micro.dem.stack.alpha	=coefficients.micro.dem.stack.alpha	,		
		coefficients.micro.dem.stack.beta	=coefficients.micro.dem.stack.beta	,
		coefficients.micro.dem.stack.gamma	=coefficients.micro.dem.stack.gamma	,
		label								=label					
		))
}	#	apc.identify.mixed
