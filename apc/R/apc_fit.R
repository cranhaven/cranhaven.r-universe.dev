#######################################################
#	apc package
#	Bent Nielen, 19 September 2016, version 1.2.3
#	functions to fit model
#######################################################
#	Copyright 2014-2016 Bent Nielsen
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
#	apc.get.design.collinear
#########################################################
apc.get.design.collinear	<- function(apc.index)
#	BN 1 April 2015
#	Constructs a collinear design matrix for an apc model.
#	It includes columns for intercept,
#	for age/period/cohort slopes,  
#	for age/period/cohort double differences.
#	Thus, there are three slopes instead of two.
#	Before use, one has to select which parameters are needed.
#	This should include at either
#	one/two of age/cohort slopes or period slope or no slope.
#	In:		apc.index			List of indices etc.
#	Out:	design.collinear	Matrix.
{	#	apc.get.design.collinear
	########################
	#	get values
	index.trap	<- apc.index$index.trap
	age.max		<- apc.index$age.max
	per.max		<- apc.index$per.max
	coh.max		<- apc.index$coh.max
	per.zero	<- apc.index$per.zero
	per.odd		<- apc.index$per.odd
	U			<- apc.index$U
	n.data		<- apc.index$n.data
	########################
	#	declaring design matrix
	p.design.collinear	<- age.max+per.max+coh.max-2
	  design.collinear	<- matrix(data=0,nrow=n.data,ncol=p.design.collinear)
	########################
	# 	construction of design matrix
	for(row in 1:n.data)
	{
		age	<- index.trap[row,1]		#	age
		coh	<- index.trap[row,2]		#	cohort
		per	<- age+coh-1
		design.collinear[row,1]	<- 1
		design.collinear[row,2]	<- age-U
		design.collinear[row,4]	<- coh-U
		design.collinear[row,3]	<- design.collinear[row,2]+design.collinear[row,4]
		if(age<U)
			design.collinear[row,(4+age):(4+U-1)]										<- seq(1,U-age)
		if(age>U+1)
			design.collinear[row,(4+U):(4+U+age-U-2)]									<- seq(age-U-1,1)
		if(per.odd && per==2*(U-1))
			design.collinear[row,(2+age.max+1)]											<- 1
		if(per>2*U)
			design.collinear[row,(2+age.max+per.odd+1):(2+age.max+per.odd+per-2*U)]		<- seq(per-2*U,1)
		if(coh<U)
			design.collinear[row,(age.max+per.max+coh):(age.max+per.max+U-1)]			<- seq(1,U-coh)
		if(coh>U+1)
			design.collinear[row,(age.max+per.max+U):(age.max+per.max+U+coh-U-2)]		<- seq(coh-U-1,1)
 	}
	return(design.collinear)
}	# 	apc.get.design.collinear

#########################################################
#	apc.get.design
#########################################################
apc.get.design	<- function(apc.index,model.design=NULL)
#	BN 27 Aug 2014
#	BN 28 Sep 2016 (7 Sep 2015): added model designs: "-A", "-P", "-C"
#	Constructs design matrix for an apc model or sub-model thereof.
#	In:		apc.index or apc.fit		List of indices etc.
#			model.design				Character. Indicates which sub-model should be fitted.
#										Default is NULL. Possible choices:
#										"APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC","1"
#										"-A","-P","-C","-AP","-AC","-CP","-APC","-tAP","-tAC","-tPC","-tA","-tP","-tC"
#										Argument is not needed when first argument is an apc.fit.
#										If not NULL it will override any information in first argument.
#	Out:	design						Matrix.
#			slopes						Vector. Length 3 of logicals,
#										indicate presence	of age/period/cohort linear slopes
#										at most two slopes can be present
#										if neither age/cohort present then period may be presents,
#										which is the case for model.design "P","tP"
#			difdif						Vector. Length 3 of logicals,
#										indicate presence	of age/period/cohort double differences
{	#	apc.get.design
	##############################
	#	check input
	if(is.null(model.design)==TRUE & is.null(apc.index$model.design)==TRUE)
		return(cat("ERROR apc.get.design: cannot find model.design\n"))
	if(is.null(model.design)==TRUE & is.null(apc.index$model.design)==FALSE)
		model.design	<- apc.index$model.design
	model.design.list		<- c("APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC","1")
	model.design.list.res	<- c("-A","-P","-C","-AP","-AC","-PC","-APC","-dAP","-dAC","-dPC","-dA","-dP","-dC","-tA","-tC")
	model.design.list	<- c(model.design.list,model.design.list.res)
	if(isTRUE(model.design %in% model.design.list)==FALSE)
		return(cat(c("ERROR apc.get.design: model.design has argument: ",model.design," which is not allowed \n")))	
	##############################
	#	get values, that are used
	age.max		<- apc.index$age.max				
	per.max		<- apc.index$per.max				
	coh.max		<- apc.index$coh.max
	##############################
	#	construct indicators for slope parameters and double difference parameters
	#	depending on model.design choice
	#		slopes:		3 vector of logicals for presence of age/period/cohort slope
	#		difdif:		3 vector of logicals for presence of age/period/cohort double differences
	if(model.design=="APC")	{	constant<-1; slopes <- c(1,0,1); difdif <- c(1,1,1);	}
	if(model.design=="AP" )	{	constant<-1; slopes <- c(1,0,1); difdif <- c(1,1,0);	}
	if(model.design=="AC" )	{	constant<-1; slopes <- c(1,0,1); difdif <- c(1,0,1);	}
	if(model.design=="PC" )	{	constant<-1; slopes <- c(1,0,1); difdif <- c(0,1,1);	}
	if(model.design=="Ad" )	{	constant<-1; slopes <- c(1,0,1); difdif <- c(1,0,0);	}
	if(model.design=="Pd" )	{	constant<-1; slopes <- c(1,0,1); difdif <- c(0,1,0);	}
	if(model.design=="Cd" )	{	constant<-1; slopes <- c(1,0,1); difdif <- c(0,0,1);	}
	if(model.design=="A"  )	{	constant<-1; slopes <- c(1,0,0); difdif <- c(1,0,0);	}
	if(model.design=="P"  )	{	constant<-1; slopes <- c(0,1,0); difdif <- c(0,1,0);	}
	if(model.design=="C"  )	{	constant<-1; slopes <- c(0,0,1); difdif <- c(0,0,1);	}
	if(model.design=="t"  )	{	constant<-1; slopes <- c(1,0,1); difdif <- c(0,0,0);	}
	if(model.design=="tA" )	{	constant<-1; slopes <- c(1,0,0); difdif <- c(0,0,0);	}
	if(model.design=="tP" )	{	constant<-1; slopes <- c(0,1,0); difdif <- c(0,0,0);	}
	if(model.design=="tC" )	{	constant<-1; slopes <- c(0,0,1); difdif <- c(0,0,0);	}
	if(model.design=="1"  )	{	constant<-1; slopes <- c(0,0,0); difdif <- c(0,0,0);	}
	if(model.design=="-A" )	{	constant<-0; slopes <- c(0,0,0); difdif <- c(1,0,0);	}
	if(model.design=="-P" )	{	constant<-0; slopes <- c(0,0,0); difdif <- c(0,1,0);	}
	if(model.design=="-C" )	{	constant<-0; slopes <- c(0,0,0); difdif <- c(0,0,1);	}
	if(model.design=="-AP")	{	constant<-0; slopes <- c(0,0,0); difdif <- c(1,1,0);	}
	if(model.design=="-AC")	{	constant<-0; slopes <- c(0,0,0); difdif <- c(1,0,1);	}
	if(model.design=="-PC")	{	constant<-0; slopes <- c(0,0,0); difdif <- c(0,1,1);	}
	if(model.design=="-APC"){   constant<-0; slopes <- c(0,0,0); difdif <- c(1,1,1);	}
	if(model.design=="-dA") {   constant<-0; slopes <- c(1,0,0); difdif <- c(1,0,0);	}
	if(model.design=="-dP") {   constant<-0; slopes <- c(0,1,0); difdif <- c(0,1,0);	}
	if(model.design=="-dC") {   constant<-0; slopes <- c(0,0,1); difdif <- c(0,0,1);	}
	if(model.design=="-dAP"){   constant<-0; slopes <- c(1,0,0); difdif <- c(1,1,0);	}
	if(model.design=="-dAC"){   constant<-0; slopes <- c(1,0,0); difdif <- c(1,0,1);	}
	if(model.design=="-dPC"){   constant<-0; slopes <- c(0,0,1); difdif <- c(0,1,1);	}
	if(model.design=="-tA") {   constant<-0; slopes <- c(1,0,0); difdif <- c(0,0,0);	}
	if(model.design=="-tC") {   constant<-0; slopes <- c(0,0,1); difdif <- c(0,0,0);	}
	##############################
	#	construct index for selecting columns of design matrix
	index.design	<- c(1)
	for(i in 1:3)	if(slopes[i])	index.design	<- c(index.design,i+1)
	if(difdif[1])	index.design <- c(index.design,4+seq(1:(age.max-2)))	
	if(difdif[2])	index.design <- c(index.design,2+age.max+seq(1:(per.max-2)))	
	if(difdif[3])	index.design <- c(index.design,  age.max+per.max+seq(1:(coh.max-2)))
	if(constant==0)	index.design <- index.design[2:length(index.design)]
	##############################
	#	get design matrix
	design	<- apc.get.design.collinear(apc.index)[,index.design]
	##############################
	return(list(	constant	= constant	,
					design		= design	,
					slopes		= slopes	,
					difdif		= difdif	
			))	
}	#	apc.get.design

#########################################################
#	apc.fit.model
#########################################################
apc.fit.model	<- function(apc.data.list,model.family,model.design,apc.index=NULL,replicate.version.1.3.1=FALSE)
#	BN 10 may 2016	od.poisson.response correction to coefficients.canonical
#	BN 14 apr 2016	Added predictors
#	BN  2 feb 2016	Changed: parameter label: date to character changed to allow nice decimal points
#					using apc.internal.function.date.2.character
#	BN 17 mar 2015
#	Function to estimate apc sub-model
#	uses canonical parametrisation as in Nielsen (2013)
#	In:		apc.data.list
#			apc.index	
#			model.family				Character
#										"poisson.response"
#											uses responses only		
#										"od.poisson.response"
#											uses responses only, over-dispersed		
#										"poisson.dose.response"
#											uses doses and responses		
#										"gaussian.response"
#											uses responses only		
#										"gaussian.rates"
#											uses rates=responses/doses only
#										"log.normal.response"
#											takes log of response and fits gaussian model
#			model.design				Character. Indicates which sub-model should be fitted.
#										Possible choices:
#										"APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC","1"
#			replicate.version.1.3.1		Logical. Replicate error in covariance calculation for
#										"poisson.response","od.poisson.response"
#										Default=FALSE
#	Out:	fit							List.	Standard output from glm.fit.
#										Note: deviance redefined in Gaussian case,
#										since glm.fit then returns RSS instead of deviance
#			coefficients.canonical		Matrix.	4 columns of coefficients, sdv, t-stat, p-values
#										with rownames for parameters
#			covariance.canonical		Matrix.	Covariance matrix, possibly using mixed parametrisation
#			slopes						Vector. Length 3 of logicals,
#										indicate presence	of age/period/cohort linear slopes
#										at most two slopes can be present
#										if neither age/cohort present then period may be presents,
#										which is the case for model.design "P","tP"
#			difdif						Vector. Length 3 of logicals,
#										indicate presence	of age/period/cohort double differences
#			index.age					Matrix. 1 column. Indices for age    double differences
#			index.per					Matrix. 1 column. Indices for period double differences
#			index.coh					Matrix. 1 column. Indices for cohort double differences
#			dates						Matrix. 1 column. dates for canonical parameters,
#										based on age1,per1,coh1,unit.  Indirectly given by user,
#										through construction of trapezoid format for responses.
#			deviance					Numeric. up to a constant, minus twice the maximized log-likelihood.
#										Where sensible, the constant is chosen so that a saturated model
#										has deviance zero.
#			RSS							Numeric. Residual sum of squares.
#										Only when model.family=="Gaussian.rates"
#			sigma2						Maximum likelihood estimator for variance (divided by n)
#										Only when model.family=="Gaussian.rates"
#			s2							Least squares estimator for variance (divided by df)
#										Only when model.family=="Gaussian.rates"
#			predictors					Vector. Design*Estimates.
#										Same as the glm.fit value linear.predictors when there is no offset
{	#	apc.fit.model
	##############################
	#	check input
	model.design.list		<- c("APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC","1")
	model.family.list		<- c("binomial.dose.response","poisson.response","od.poisson.response","poisson.dose.response","gaussian.rates","gaussian.response","log.normal.rates","log.normal.response")
	model.family.gaussian	<- c("gaussian.rates","gaussian.response","log.normal.rates","log.normal.response")
	model.family.mixed		<- c("poisson.response","od.poisson.response")
	if(isTRUE(model.design %in% model.design.list)==FALSE)
		return(cat("ERROR apc.fit.model: model.design has wrong argument \n"))	
	if(isTRUE(model.family %in% model.family.list)==FALSE)
		return(cat("ERROR apc.fit.model: model.family has wrong argument \n"))
	######################
	#	get index
	if(is.null(apc.index)==TRUE)
		apc.index	<- apc.get.index(apc.data.list)
	##############################
	#	create indicator for mixed parametrisation
	mixed.par	<- (1-isTRUE(model.design=="1"))*isTRUE(model.family %in% model.family.mixed)
	mixed.par.1	<-    isTRUE(model.design=="1") *isTRUE(model.family %in% model.family.mixed)
	##############################
	#	get values, that are used
	age.max		<- apc.index$age.max				
	per.max		<- apc.index$per.max				
	coh.max		<- apc.index$coh.max
	age1		<- apc.index$age1    
	per1		<- apc.index$per1
	coh1		<- apc.index$coh1    
	unit		<- apc.index$unit
	per.zero	<- apc.index$per.zero
	index.trap	<- apc.index$index.trap
	n.data		<- apc.index$n.data
	v.response	<- apc.index$response[apc.index$index.data]
	v.dose		<- apc.index$dose[    apc.index$index.data]
	n.decimal	<- apc.index$n.decimal
	##############################
	#	get design
	#		design matrix 
	#		slopes:		3 vector of logicals for presence of age/period/cohort slope
	#		difdif:		3 vector of logicals for presence of age/period/cohort double differences
	get.design	<- apc.get.design(apc.index,model.design)
	design		<- get.design$design
	slopes		<- get.design$slopes
	difdif		<- get.design$difdif
	##############################
	#	REGRESSION
	#	Binomial/logistic regression with logit link
	if(model.family=="binomial.dose.response")
		fit	<- glm.fit(design,cbind(v.response,v.dose-v.response),family=binomial(link="logit"))
	#	Poisson regression for response only and with log link
	if(model.family=="poisson.response")
		fit	<- glm.fit(design,v.response,family=poisson(link="log"))	
	#	Poisson regression for response only and with log link
	if(model.family=="od.poisson.response")
	{	#	Note: quasipoisson family avoids warnings with non integer observations
		#	But, when it later comes to computing sdv, we go back to poisson.
		fit	<- glm.fit(design,v.response,family=quasipoisson(link="log"))
		fit$family$family	<- "poisson"			
	}
	#	Poisson regression for dose-response and with log link
	if(model.family=="poisson.dose.response")
		fit	<- glm.fit(design,v.response,family=poisson(link="log"),offset=log(v.dose))
	#	Gaussian regression for response only and with identity link (Least Squares)
	if(model.family=="gaussian.response")
		fit	<- glm.fit(design,v.response,family=gaussian(link="identity"))
	#	Gaussian regression for rates and with identity link (Least Squares)
	if(model.family=="gaussian.rates")
		fit	<- glm.fit(design,v.response/v.dose,family=gaussian(link="identity"))		
	#	Gaussian regression for log(response) and with identity link (Least Squares)
	if(model.family=="log.normal.response")
		fit	<- glm.fit(design,log(v.response),family=gaussian(link="identity"))
	#	Gaussian regression for log(rates) and with identity link (Least Squares)
	if(model.family=="log.normal.rates")
		fit	<- glm.fit(design,log(v.response/v.dose),family=gaussian(link="identity"))		
	##############################
	#	construct for indices for double difference parameters  
	index.age	<- NULL
	index.per	<- NULL
	index.coh	<- NULL
	start		<- 1+sum(slopes)
	if(difdif[1])	{	index.age	<- start+seq(1,age.max-2);	start	<- start+age.max-2	}
	if(difdif[2])	{	index.per	<- start+seq(1,per.max-2);	start	<- start+per.max-2	}
	if(difdif[3])	{	index.coh	<- start+seq(1,coh.max-2);	start	<- start+coh.max-2	}
	xi.dim		<- start
	##############################
	#	construct dates for for double difference parameters
	dates		<- matrix(data=NA,nrow=xi.dim,ncol=1)			
	if(difdif[1])	dates[index.age,1]	<- age1+seq(2,age.max-1)*unit	
	if(difdif[2])	dates[index.per,1]	<- per1+seq(2,per.max-1)*unit
	if(difdif[3])	dates[index.coh,1]	<- coh1+seq(2,coh.max-1)*unit
	##############################
	#	construct row names for canonical parameter
	names	<- c("level")
	if(slopes[1])	names	<- c(names,"age slope")
	if(slopes[2])	names	<- c(names,"period slope")
	if(slopes[3])	names	<- c(names,"cohort slope")
	if(difdif[1])
		for(i in 1:(age.max-2))
			names	<- c(names,paste("DD_age_"   ,apc.internal.function.date.2.character((dates[index.age,1])[i],n.decimal),sep=""))
			if(difdif[2])
		for(i in 1:(per.max-2))
			names	<- c(names,paste("DD_period_",apc.internal.function.date.2.character((dates[index.per,1])[i],n.decimal),sep=""))
	if(difdif[3])
		for(i in 1:(coh.max-2))
			names	<- c(names,paste("DD_cohort_",apc.internal.function.date.2.character((dates[index.coh,1])[i],n.decimal),sep=""))
	##############################
	#	Get coefficients and covariance
	coefficients.canonical	<- summary.glm(fit)$coefficients
	rownames(coefficients.canonical)	<- names
  	if (model.family == "od.poisson.response") 
    	colnames(coefficients.canonical) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")     
	covariance.canonical	<- summary.glm(fit)$cov.scaled
	#	Need to condition in mixed parametrisation
	if(mixed.par && replicate.version.1.3.1)
	{
		c22	<- covariance.canonical[2:xi.dim,2:xi.dim]
		c21	<- covariance.canonical[2:xi.dim,1]
		c11	<- covariance.canonical[1,1]
		covariance.canonical[2:xi.dim,2:xi.dim]	<- c22 - c21 %o% c21 / c11
	}
	if(mixed.par | mixed.par.1)
	{
		covariance.canonical[1,]	<- 0
		covariance.canonical[,1]	<- 0		
	}	
	##############################
	#	get standard errors 
	coefficients.canonical[,2]	<- sqrt(diag(covariance.canonical))
	if(model.family=="od.poisson.response")
		coefficients.canonical[,2] <- coefficients.canonical[,2] * sqrt(fit$deviance/fit$df.residual)		
	if(mixed.par | mixed.par.1)		# mixed parametrisation
		coefficients.canonical[1,2]	<- NA
	##############################
	#	get t-statistics
	coefficients.canonical[,3]	<- coefficients.canonical[,1] 	/ coefficients.canonical[,2]
	##############################
	#	get p-values
	coefficients.canonical[,4]	<- 2*pnorm(abs(coefficients.canonical[  ,3]),lower.tail=FALSE)
	if(model.family=="od.poisson.response")
		coefficients.canonical[,4]	<- 2*pt(abs(coefficients.canonical[  ,3]),fit$df.residual,lower.tail=FALSE)		
	##############################
	#	get deviance, RSS and variance estimate in Gaussian case
	#	note that glm.fit returns RSS instead of deviance in Gaussian case
	RSS		<- NULL
	sigma2	<- NULL
	s2		<- NULL
	if(isTRUE(model.family %in% model.family.gaussian))
	{
		RSS				<- fit$deviance			#	GLM deviance for gaussian = RSS
		sigma2			<- RSS/n.data
		s2				<- RSS/fit$df.residual
		fit$deviance	<- n.data*(1+log(2*pi)+log(sigma2))
	}	
	##############################
	#	predictors 14 apr 2016	
	predictors	<- (design %*% summary.glm(fit)$coefficients)[,1]
	##############################
	return(c(fit,apc.index,
				list(	model.family				= model.family					,
						model.design				= model.design					,
						coefficients.canonical		= coefficients.canonical		,
						covariance.canonical		= covariance.canonical			,
						slopes						= slopes						,
						difdif						= difdif						,
						index.age					= index.age 					,
						index.per					= index.per 					,
						index.coh					= index.coh 					,
						dates						= dates							,
						RSS							= RSS							,
						sigma2						= sigma2						,
						s2							= s2							,
						n.decimal					= n.decimal						,
						predictors					= predictors			
			)))	
}	#	apc.fit.model

#########################################################
#	apc.fit.table
#########################################################
apc.fit.table	<- function(apc.data.list,model.family,model.design.reference="APC",apc.index=NULL)
#	BN 10 mar 2018: added F test to gaussian family
#	BN 17 may 2016: labels corrected
#	BN 10 may 2016: reference model introduced
#	BN 17 mar 2015
{	#	apc.fit.table
	######################
	#	model families
	model.family.gaussian	<- c("gaussian.rates","gaussian.response","log.normal.rates","log.normal.response")
	model.family.od			<- c("od.poisson.response")
	model.family.other		<- c("binomial.dose.response","poisson.response","poisson.dose.response")
	model.family.list		<- c(model.family.gaussian,model.family.od,model.family.other)

	######################
	#	check input
	if(isTRUE(model.family %in% model.family.list)==FALSE)
		return(cat("apc.fit.table error: model.family has wrong argument \n"))
	######################
	#	get index
	if(is.null(apc.index)==TRUE)
		apc.index	<- apc.get.index(apc.data.list)
	######################
	#	Function to get one line of table from two fits
	fit.tab.line.glm	<- function(fit.U,fit.R)	
	#	BN 12 Mar 2018 	updated to simplify output
	#	BN 20 Sep 2013
	#	returns
	#			dev.R		deviance restricted model
	#			df.R		degrees of freedom restricted model
	#			pchisq		deviance test (if not gaussian)
	#			LR			likelihood ratio 
	#			df			degrees of freedom difference 
	#			aic
	{
		model.family<- fit.U$model.family
		dev.U		<- fit.U$deviance
		dev.R		<- fit.R$deviance
		df.U		<- fit.U$df.residual
		df.R		<- fit.R$df.residual
		RSS.U		<- fit.U$RSS
		RSS.R		<- fit.R$RSS
		aic			<- fit.R$aic
		p.dev		<- pchisq(dev.R,df.R,lower.tail=FALSE)
		if(fit.U$model.design==fit.R$model.design)
		{
			LR <- df <- p.LR	<- F.odp <- F.gauss <- p.F.odp <- p.F.gauss <- NaN
		}
		else
		{
			LR			<- dev.R-dev.U
			df			<- df.R-df.U			
			p.LR		<- pchisq(LR,df,lower.tail=FALSE)
			F.odp		<- (LR/df)/(dev.U/df.U)
			F.gauss 	<- ((RSS.R-RSS.U)/df)/(RSS.U/df.U)
			p.F.odp		<- pf(F.odp  ,df,df.U,lower.tail=FALSE)
			p.F.gauss	<- pf(F.gauss,df,df.U,lower.tail=FALSE)
		}	
		if(model.family %in% model.family.gaussian)
			#	-2logL , df , LR , df.res , p.LR , F , p.F, aic	 = 8
			return(round(c(dev.R,df.R,LR,df,p.LR,F.gauss,p.F.gauss,aic),digits=3))
		if(model.family  %in% model.family.od)
			#	dev    , df , F.odp , df.res, p.F.odp			 = 6
			return(round(c(dev.R,df.R,p.dev,F.odp,df,p.F.odp),digits=3))
		if(model.family  %in% model.family.other)
			#	dev		, df , LR , df.res , p.LR , aic			 = 7	
			return(round(c(dev.R,df.R,p.dev,LR,df,p.LR,aic),digits=3))
	}
	######################
	if(model.design.reference=="APC")	model.design.list	<- c("APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC","1")
	if(model.design.reference=="AP")	model.design.list	<- c( 	   "AP",		  "Ad","Pd", 	 "A","P",    "t","tA","tP",     "1")
	if(model.design.reference=="AC")	model.design.list	<- c(			"AC",	  "Ad",		"Cd","A",	 "C","t","tA",	   "tC","1")
	if(model.design.reference=="PC")	model.design.list	<- c(				 "PC",	   "Pd","Cd",	 "P","C","t",	  "tP","tC","1")
	if(model.design.reference=="Ad")	model.design.list	<- c( 	   				  "Ad",		 	 "A",	     "t","tA",		    "1")
	if(model.design.reference=="Pd")	model.design.list	<- c( 	   				  	   "Pd", 	 	 "P",    "t", 	  "tP",     "1")
	if(model.design.reference=="Cd")	model.design.list	<- c(				 				"Cd",	 	 "C","t",	  	   "tC","1")
	if(model.design.reference=="A")		model.design.list	<- c( 	   							  	 "A",	     	 "tA",		    "1")
	if(model.design.reference=="P")		model.design.list	<- c( 	   						  	 		 "P",     	 	  "tP",     "1")
	if(model.design.reference=="C")		model.design.list	<- c(				 					 	 	 "C",	  	 	   "tC","1")
	if(model.design.reference=="t")		model.design.list	<- c(				 								 "t","tA","tP","tC","1")
	######################	
	#	number of columns
		if(isTRUE(model.family %in% model.family.gaussian))	ncol <- 8
		if(isTRUE(model.family %in% model.family.od))		ncol <- 6
		if(isTRUE(model.family %in% model.family.other))	ncol <- 7
	######################
	#	declare table
	fit.tab		<- matrix(nrow=length(model.design.list),ncol=ncol,data=NA)
	#	unrestricted apc model
	fit.ref	<- apc.fit.model(apc.data.list,model.family,model.design=model.design.reference,apc.index)	
	#	model list
	for(i in 1:length(model.design.list))
	{
		fit.sub			<- apc.fit.model(apc.data.list,model.family,model.design=model.design.list[i],apc.index)
		fit.tab[i,]		<- fit.tab.line.glm(fit.ref, fit.sub)
	}
	#	row names
	rownames(fit.tab)	<- model.design.list
	#	column names
	if(isTRUE(model.family %in% model.family.gaussian))
		colnames(fit.tab)	<- c("-2logL","df.residual",paste("LR vs.",model.design.reference,sep=""),paste("df vs.",model.design.reference,sep=""),"prob(>chi_sq)",paste("F vs.",model.design.reference,sep=""),"prob(>F)","aic")
	if(isTRUE(model.family %in% model.family.od))
		colnames(fit.tab)	<- c("deviance","df.residual","prob(>chi_sq)",paste("F vs.",model.design.reference,sep=""),paste("df vs.",model.design.reference,sep=""),"prob(>F)")
	if(isTRUE(model.family %in% model.family.other))
		colnames(fit.tab)	<- c("deviance","df.residual","prob(>chi_sq)",paste("LR vs.",model.design.reference,sep=""),paste("df vs.",model.design.reference,sep=""),"prob(>chi_sq)","aic")
	######################
	return(fit.tab)
}	#	apc.fit.table
# 	new.apc.fit.table(data,"od.poisson.response")
#	data.paid	<- data.loss.XL()
#	apc.fit.table(data.paid,"od.poisson.response")
#	apc.fit.table(data,"poisson.response")
