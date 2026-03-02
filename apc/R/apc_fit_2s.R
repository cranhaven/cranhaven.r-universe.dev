#######################################################
#	apc package
#	Bent Nielsen, 12 Dec 2023, version 3.0.0
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

#########################################################
#	apc.fit.model.2s
#########################################################
apc.fit.model.2s	<- function(apc.data.list.1,apc.data.list.2,model.family,model.design.common="APC",model.design.difference="APC",gls.weight=c(1,1),apc.index=NULL,time.series=NULL)
#	BN 12 Dec 2023
#	Function to estimate 2-sample apc sub-model
#	Allow adding external time series instead of period effect
#	uses canonical parametrisation as in Nielsen (2023)
#	In:		apc.data.list.1
#			apc.data.list.2				Data lists. With different response, dose, but
#										otherwise the same input	
#			model.family				Character
#										"log.normal.response"
#											takes log of response and fits gaussian model
#										"log.normal.rates"
#											uses rates=responses/doses only
#											takes log of rates and fits gaussian model
#										"gls.log.normal.rates"
#											GLS regression.
#											divides log rates and design for each sample
#											with respective weights.
#											Note, F-statistics are the same as with
#											log normal rates
#											With small-dispersion asymptotics the
#											F statistics are asymptotically F.
#			model.design.common			Character. Indicates which sub-model should be fitted
#										for the common part that is for the sum of the
#										canonical parameters (xi.1+xi.2)/2.
#										Possible choices:
#										"APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC"
#			model.design.difference		Character. Indicates which sub-model should be fitted
#										for the common part that is for the sum of the
#										canonical parameters (xi.1+xi.2)/2.
#										Possible choices:
#										"APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC"
#			gls.weight					Vector, length 2.
#										GLS regression divides log(rates) and design
#										for each sample by respective weights
#										Weights could be their residual standard deviation
#										in 1-sample analysis
#										Or, Weights could be normalized so that
#										2nd element is 1
#										1st element is 1-sample residual standard deviation
#										for 1st sample divided by that of 2nd sample
#										deviation for the first sample and 1 for second sample
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
{	#	apc.fit.model.2s
	######################
	#	get index
	if(is.null(apc.index)==TRUE)
		apc.index	<- apc.get.index(apc.data.list.1)
	##############################
	#	get values, that are used
	age.max		  <- apc.index$age.max				
	per.max		  <- apc.index$per.max				
	coh.max	  	<- apc.index$coh.max
	age1		    <- apc.index$age1    
	per1		    <- apc.index$per1
	coh1	  	  <- apc.index$coh1    
	unit  		  <- apc.index$unit
	per.zero	  <- apc.index$per.zero
	index.data  <- apc.index$index.data
	index.trap  <- apc.index$index.trap
	n.decimal	  <- apc.index$n.decimal
	data.format	<- apc.index$data.format
	n.data.1	  <- apc.index$n.data
	n.data		  <- 2*n.data.1
	v.response	<- c(apc.data.list.1$response[index.data],apc.data.list.2$response[index.data])
	v.dose		  <- c(apc.data.list.1$dose[    index.data],apc.data.list.2$dose[    index.data])
				
	##############################
	#	check input
	data.format.list.regular<- c("AP","AC","CA","CL","CP","PA","PC","trap","trapezoid")	# BN 11 Dec 2023
	data.format.list.mixed	<- c("APm","PAm")											# BN 11 Dec 2023
	model.design.list		    <- c("APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC","ATC")
	model.family.gls		    <- c("gls.log.normal.rates")
	model.family.gaussian	  <- c("log.normal.rates","log.normal.response",model.family.gls)
	model.family.mixed		  <- NULL
	model.family.list		    <- c(model.family.gaussian)
	if(isTRUE(model.design.common %in% model.design.list)==FALSE)
		return(cat("ERROR apc.fit.model: model.design.common has wrong argument \n"))	
	if(isTRUE(model.design.difference %in% model.design.list)==FALSE)
		return(cat("ERROR apc.fit.model.2s: model.design.difference has wrong argument \n"))	
	if(isTRUE(model.family %in% model.family.list)==FALSE)
		return(cat("ERROR apc.fit.model.2s: model.family has wrong argument \n"))
	if(model.family %in% model.family.gls && isTRUE(length(gls.weight)==2)==FALSE)	
		return(cat("ERROR apc.fit.model.2s: with GLS model.family the gls.weight must be vector of length 2 \n"))
	if(model.design.common=="ATC" && is.null(time.series)==TRUE)		# 25 Jan 2024 time series
		stop("model.design.common=ATC requires time.series argument")
	if(model.design.difference=="ATC" && is.null(time.series)==TRUE)		# 25 Jan 2024 time series
		stop("model.design.difference=ATC requires time.series argument")
	if(length(time.series)!=per.max && is.null(time.series)==FALSE)     # 4 Jul 2025 added time series check 
		stop("time.series should have same length as period dimension")
	######################
	#	local def of model design
	# 	25 Jan 2024 time series
	l.model.design.com	<- model.design.common
	l.model.design.dif	<- model.design.difference
	if(l.model.design.com=="ATC") 	l.model.design.com	<- "AC"
	if(l.model.design.dif=="ATC") 	l.model.design.dif	<- "AC"
	##############################
	#	create indicator for mixed parametrisation
	mixed.par	<- 0
	if(model.family %in% model.family.mixed)	{	mixed.par <- 1	}
	########################
	#	some derived values
	G	<- H	<- 1
	if(data.format %in% data.format.list.mixed)	{	# 11 Dec 2023
		G	<- unit[2];
		H	<- unit[3];
		N.coh	<- function.coin(G,H)$non.representable;
		I0.coh	<- setdiff((G+2*H):C,c(C-N.coh,G+2*H+N.coh));	
	}	
	A	<- age.max*G;
	P	<- per.max*H;
	C	<- A+P-G
	N.coh	<- function.coin(G,H)$non.representable
	I0.coh	<- setdiff((G+2*H):C,c(C-N.coh,G+2*H+N.coh))	  
	##############################
	#	get design for common and difference blocks
	#		design matrices for common and difference
	#		design overall
	#		slopes:		3 vector of logicals for presence of age/period/cohort slope
	#		difdif:		3 vector of logicals for presence of age/period/cohort double differences
	get.design.com	<- apc.get.design(apc.index,l.model.design.com)
	design.com		<- get.design.com$design
	slopes.com		<- get.design.com$slopes
	difdif.com		<- get.design.com$difdif
	get.design.dif	<- apc.get.design(apc.index,l.model.design.dif)
	design.dif		<- get.design.dif$design
	slopes.dif		<- get.design.dif$slopes
	difdif.dif		<- get.design.dif$difdif
	##############################
	#	construct for indices for double difference parameters  
	index.age.com	<- NULL
	index.per.com	<- NULL
	index.coh.com	<- NULL
	start		<- 1+slopes.com[1]*G+slopes.com[2]*H+slopes.com[3]										
	if(difdif.com[1])	{	index.age.com	<- start+seq(1,age.max-H-1);	start	<- start+age.max-H-1	}	
	if(difdif.com[2])	{	index.per.com	<- start+seq(1,per.max-G-1);	start	<- start+per.max-G-1	}	
	if(difdif.com[3])	{	index.coh.com	<- start+I0.coh-G-2*H+1;		start	<- start+length(I0.coh)	}	
	xi.dim.com		<- start
	index.age.dif	<- NULL
	index.per.dif	<- NULL
	index.coh.dif	<- NULL
	start		<- 1+slopes.dif[1]*G+slopes.dif[2]*H+slopes.dif[3]										
	if(difdif.dif[1])	{	index.age.dif	<- start+seq(1,age.max-H-1);	start	<- start+age.max-H-1	}	
	if(difdif.dif[2])	{	index.per.dif	<- start+seq(1,per.max-G-1);	start	<- start+per.max-G-1	}	
	if(difdif.dif[3])	{	index.coh.dif	<- start+I0.coh-G-2*H+1;		start	<- start+length(I0.coh)	}	
	xi.dim.dif		<- start
	##############################
	#	Adjust design with time series
	# 3 Jul 2025: in def of DGD.time.series replaced i.F with i.time.series
	#	25 Jan 2024
	if(model.design.common=="ATC" || model.design.difference=="ATC")	{
		i.time.series	<- (G+2):per.max
		DGD.time.series	<- time.series[i.time.series] - time.series[i.time.series-H] - time.series[i.time.series-G] + time.series[i.time.series-G-H]
		design.apc	<- apc.get.design(apc.index,"APC")$design
		start			<- 1+slopes.dif[1]*G+slopes.dif[2]*H+slopes.dif[3]
		start			<- start+age.max-H-1		
		index.per.apc	<- start+seq(1,per.max-G-1)
	}	
	if(model.design.common=="ATC")	{
		design.com	<- cbind(design.com, design.apc[,index.per.apc] %*% DGD.time.series)
		xi.dim.com	<- xi.dim.com+1
		index.per	<- xi.dim.com
	}	
	if(model.design.difference=="ATC")	{
		design.dif	<- cbind(design.dif, design.apc[,index.per.apc] %*% DGD.time.series)
		xi.dim.dif	<- xi.dim.dif+1
		index.per	<- xi.dim.dif
	}		
	##############################
	#	get design for 2-sample model
	design			<- rbind(cbind(design.com,design.dif),cbind(design.com,-design.dif))
	##############################
	#	REGRESSION
	#	Gaussian regression for log(response) and with identity link (Least Squares)
	if(model.family=="log.normal.response")
		fit	<- glm.fit(design,log(v.response),family=gaussian(link="identity"))
	#	Gaussian regression for log(rates) and with identity link (Least Squares)
	if(model.family=="log.normal.rates")
		fit	<- glm.fit(design,log(v.response/v.dose),family=gaussian(link="identity"))		
	#	GLS Gaussian regression for log(rates) and with identity link (Least Squares)
	#	where rates and design are weighted differently for each sample
	if(model.family=="gls.log.normal.rates")	{
		gls.design	<- design;
		gls.rates	<- log(v.response/v.dose);
		gls.design[1:n.data.1,]			<- gls.design[1:n.data.1,]/gls.weight[1];
		gls.rates[ 1:n.data.1 ]			<- gls.rates[ 1:n.data.1 ]/gls.weight[1];
		gls.design[(n.data.1+1):n.data,]<- gls.design[(n.data.1+1):n.data,]/gls.weight[2];
		gls.rates[ (n.data.1+1):n.data ]<- gls.rates[ (n.data.1+1):n.data ]/gls.weight[2];
		fit	<- glm.fit(gls.design,gls.rates,family=gaussian(link="identity"));
		}	
	##############################
	#	construct dates for for double difference parameters
	dates.com		<- matrix(data=NA,nrow=xi.dim.com,ncol=1)			
	if(difdif.com[1])	dates.com[index.age.com,1]	<- age1+seq(H+1,age.max-1)*G*unit[1]
	if(difdif.com[2])	dates.com[index.per.com,1]	<- per1+seq(G+1,per.max-1)*H*unit[1]
	if(difdif.com[3])	dates.com[index.coh.com,1]	<- coh1+        (I0.coh-1)  *unit[1]
	dates.dif		<- matrix(data=NA,nrow=xi.dim.dif,ncol=1)			
	if(difdif.dif[1])	dates.dif[index.age.dif,1]	<- age1+seq(H+1,age.max-1)*G*unit[1]
	if(difdif.dif[2])	dates.dif[index.per.dif,1]	<- per1+seq(G+1,per.max-1)*H*unit[1]
	if(difdif.dif[3])	dates.dif[index.coh.dif,1]	<- coh1+        (I0.coh-1)  *unit[1]
	##############################
	#	construct row names for canonical parameter
	names.com	<- c("com_level")
	if(data.format %in% data.format.list.regular)	{	# 12 Dec 2023
		if(slopes.com[1])	{	names.com	<- c(names.com,"com_age slope")	};
		if(slopes.com[2])	{	names.com	<- c(names.com,"com_per slope")	};
		if(slopes.com[3])	{	names.com	<- c(names.com,"com_coh slope")	};
		}	
	if(data.format %in% data.format.list.mixed)		{	# 12 Dec 2023
		if(slopes.com[1])	{	names.com	<- c(names.com,paste("com_age_slope_",as.character(1:H),sep=""))	};
		if(slopes.com[2])	{	names.com	<- c(names.com,paste("com_per_slope_",as.character(1:G),sep=""))	};
		}
	if(difdif.com[1])	{	names.com	<- c(names.com,paste("com_D",as.character(G*H),"D",as.character(G),"_age_",apc.internal.function.date.2.character(dates.com[index.age.com,1]),sep=""))	};	
	if(difdif.com[2])	{	names.com	<- c(names.com,paste("com_D",as.character(G*H),"D",as.character(H),"_per_",apc.internal.function.date.2.character(dates.com[index.per.com,1]),sep=""))	};	
	if(difdif.com[3])	{	names.com	<- c(names.com,paste("com_D",as.character(G)  ,"D",as.character(H),"_coh_",apc.internal.function.date.2.character(dates.com[index.coh.com,1]),sep=""))	};
	if(model.design.common=="ATC")	names.com	<- c(names.com,"t.s.")	# 25 Jan 2024 time series
	names.dif	<- c("dif_level")
	if(data.format %in% data.format.list.regular)	{	# 12 Dec 2023
		if(slopes.dif[1])	{	names.dif	<- c(names.dif,"dif_age slope")	};
		if(slopes.dif[2])	{	names.dif	<- c(names.dif,"dif_per slope")	};
		if(slopes.dif[3])	{	names.dif	<- c(names.dif,"dif_coh slope")	};
		}	
	if(data.format %in% data.format.list.mixed)		{	# 12 Dec 2023
		if(slopes.dif[1])	{	names.dif	<- c(names.dif,paste("dif_age_slope_",as.character(1:H),sep=""))	};
		if(slopes.dif[2])	{	names.dif	<- c(names.dif,paste("dif_per_slope_",as.character(1:G),sep=""))	};
		}
	if(difdif.dif[1])	{	names.dif	<- c(names.dif,paste("dif_D",as.character(G*H),"D",as.character(G),"_age_",apc.internal.function.date.2.character(dates.dif[index.age.dif,1]),sep=""))	};	
	if(difdif.dif[2])	{	names.dif	<- c(names.dif,paste("dif_D",as.character(G*H),"D",as.character(H),"_per_",apc.internal.function.date.2.character(dates.dif[index.per.dif,1]),sep=""))	};	
	if(difdif.dif[3])	{	names.dif	<- c(names.dif,paste("dif_D",as.character(G)  ,"D",as.character(H),"_coh_",apc.internal.function.date.2.character(dates.dif[index.coh.dif,1]),sep=""))	};	
	if(model.design.difference=="ATC")	names.dif	<- c(names.dif,"t.s.")	# 25 Jan 2024 time series
	##############################
	#	Get coefficients and covariance
	coefficients.canonical	<- summary.glm(fit)$coefficients	
	rownames(coefficients.canonical)	<- c(names.com,names.dif)
  	if (model.family == "od.poisson.response") 
    	colnames(coefficients.canonical) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")     
	covariance.canonical	<- summary.glm(fit)$cov.scaled
	#	Need to condition in mixed parametrisation
	if(mixed.par)
	{
		covariance.canonical[1,]	<- 0
		covariance.canonical[,1]	<- 0		
	}	
	##############################
	#	get standard errors 
	coefficients.canonical[,2]	<- sqrt(diag(covariance.canonical))
	if(model.family=="od.poisson.response")
		coefficients.canonical[,2] <- coefficients.canonical[,2] * sqrt(fit$deviance/fit$df.residual)		
	if(mixed.par)		# mixed parametrisation
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
	return(c(fit,apc.index,
				list(	model.family				= model.family					,
						model.2s					= TRUE							,
						model.design.common			= model.design.common			,
						model.design.difference		= model.design.difference		,
						coefficients.canonical		= coefficients.canonical		,
						covariance.canonical		= covariance.canonical			,
						slopes.com					= slopes.com					,
						slopes.dif					= slopes.dif					,
						difdif.com					= difdif.com					,
						difdif.dif					= difdif.dif					,
						index.age.com				= index.age.com 				,
						index.age.dif				= index.age.dif 				,
						index.per.com				= index.per.com 				,
						index.per.dif				= index.per.dif 				,
						index.coh.com				= index.coh.com 				,
						index.coh.dif				= index.coh.dif 				,
						dates.com					= dates.com						,
						dates.dif					= dates.dif						,
						RSS							= RSS							,
						sigma2						= sigma2						,
						s2							= s2							#,
						#n.decimal					= n.decimal						,
						#predictors					= predictors			
			)))	
}	#	apc.fit.model.2s

#########################################################
#	apc.fit.table.2s
#########################################################
apc.fit.table.2s	<- function(apc.data.list.1,apc.data.list.2,model.family,restrict="difference",model.design.reference.common="APC",model.design.reference.difference="APC",gls.weight=c(1,1),time.series=NULL,digits=3)
#	BN 12 Dec 2023model.family.od
#	Function to get deviance table for 2-sample apc sub-model
#	In:		restrict			String. Either "difference" or "common"
#								Which type of parameter is restricted?
#								Default "difference"
#			digits				Numeric.  Number of digits in output
#								Default 3.
{	#	apc.fit.table.2s
	######################
	#	model families
	model.family.gls		  <- c("gls.log.normal.rates")
	model.family.gaussian	<- c("log.normal.rates","log.normal.response",model.family.gls)
# 	model.family.od			  <- NULL
# 	model.family.other		<- NULL
	model.family.list		  <- c(model.family.gaussian)
	######################
	#	check input
	if(isTRUE(model.family %in% model.family.list)==FALSE)
		return(cat("apc.fit.table.2s error: model.family has wrong argument \n"))
	if(isTRUE(restrict %in% c("common","difference"))==FALSE)
		return(cat("apc.fit.table.2s error: restrict should have argument 'difference' or 'common' \n"))
	######################
	#	get index
	apc.index	<- apc.get.index(apc.data.list.1)
	######################
	#	Function to get one line of table from two fits
	fit.tab.line.glm	<- function(fit.U,fit.R,restrict)	
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
		if(restrict=="common")	{
			U.model.design	<- fit.U$model.design.common;
			R.model.design	<- fit.R$model.design.common;
		}	
		if(restrict=="difference")	{
			U.model.design	<- fit.U$model.design.difference;
			R.model.design	<- fit.R$model.design.difference;
		}	
		
		if(U.model.design==R.model.design)	{
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
			#	-2logL , df , LR , df.res , p.LR , F , p.F, aic, RSS, s = 10
			return(round(c(dev.R,df.R,LR,df,p.LR,F.gauss,p.F.gauss,aic,RSS.R,sqrt(RSS.R/df.R)),digits=digits))
		# if(model.family  %in% model.family.od)
		# 	#	dev    , df , F.odp , df.res, p.F.odp			 = 6
		# 	return(round(c(dev.R,df.R,p.dev,F.odp,df,p.F.odp),digits=digits))
		# if(model.family  %in% model.family.other)
		# 	#	dev		, df , LR , df.res , p.LR , aic			 = 7	
		# 	return(round(c(dev.R,df.R,p.dev,LR,df,p.LR,aic),digits=digits))
	}
	######################
	if(restrict=="difference")	{
		model.design.reference	<- model.design.reference.difference;
		print("restrictions on difference parameter")}
	if(restrict=="common")		{
		model.design.reference	<- model.design.reference.common;
		print("restrictions on common parameter")}		
	######################
	if(is.null(time.series)==TRUE)	{
	if(model.design.reference=="APC")	model.design.list	<- c("APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t","tA","tP","tC")
	if(model.design.reference=="AP")	model.design.list	<- c( 	   "AP",		      "Ad","Pd", 	   "A","P",    "t","tA","tP"     )
	if(model.design.reference=="AC")	model.design.list	<- c(			      "AC",	    "Ad",		  "Cd","A",	   "C","t","tA",	   "tC")
	if(model.design.reference=="PC")	model.design.list	<- c(				         "PC",	   "Pd","Cd",	   "P","C","t",	    "tP","tC")
	if(model.design.reference=="Ad")	model.design.list	<- c( 	   				        "Ad",		 	     "A",	       "t","tA" 		     )
	if(model.design.reference=="Pd")	model.design.list	<- c( 	   				  	         "Pd", 	 	     "P",    "t", 	  "tP"     )
	if(model.design.reference=="Cd")	model.design.list	<- c(				 				                "Cd",	 	     "C","t",	  	     "tC")
	if(model.design.reference=="A")		model.design.list	<- c( 	   							  	             "A",	     	     "tA"		       )
	if(model.design.reference=="P")		model.design.list	<- c( 	   						  	 		               "P",     	 	    "tP"     )
	if(model.design.reference=="C")		model.design.list	<- c(				 					 	 	                       "C",	  	 	       "tC")
	if(model.design.reference=="t")		model.design.list	<- c(				 								                         "t","tA","tP","tC")
	}
	if(is.null(time.series)==FALSE)	{
	if(model.design.reference=="APC")	model.design.list	<- c("APC","ATC","AC")
	if(model.design.reference=="ATC")	model.design.list	<- c(      "ATC","AC")
	}
	######################	
	#	number of columns
		if(isTRUE(model.family %in% model.family.gaussian))	ncol <- 10	# changed from 8 to 10 to include RSS and sigma^2
# 		if(isTRUE(model.family %in% model.family.od))		  ncol <- 6
# 		if(isTRUE(model.family %in% model.family.other))	ncol <- 7
  ######################
	#	declare table
	fit.tab		<- matrix(nrow=length(model.design.list),ncol=ncol,data=NA)
	#	unrestricted apc model
	fit.ref	<- apc.fit.model.2s(apc.data.list.1,apc.data.list.2,model.family,model.design.reference.common,model.design.reference.difference,gls.weight,apc.index,time.series)
	#	model list
	for(i in 1:length(model.design.list))
	{
		if(restrict=="common")	{
			fit.sub			<- apc.fit.model.2s(apc.data.list.1,apc.data.list.2,model.family,model.design.list[i],model.design.reference.difference,gls.weight,apc.index,time.series)	
		}
		if(restrict=="difference")	{
			fit.sub			<- apc.fit.model.2s(apc.data.list.1,apc.data.list.2,model.family,model.design.reference.common,model.design.list[i],gls.weight,apc.index,time.series)	
		}
		fit.tab[i,]		<- fit.tab.line.glm(fit.ref, fit.sub,restrict)
	}
	#	row names
	if(restrict=="common")
		rownames(fit.tab)	<- paste(model.design.list,model.design.reference.difference)
	if(restrict=="difference")
		rownames(fit.tab)	<- paste(model.design.reference.common,model.design.list)
	#	column names
	if(isTRUE(model.family %in% model.family.gaussian))
		colnames(fit.tab)	<- c("-2logL","df.residual",paste("LR vs.",model.design.reference,sep=""),paste("df vs.",model.design.reference,sep=""),"prob(>chi_sq)",paste("F vs.",model.design.reference,sep=""),"prob(>F)","aic","RSS","sigma")
# 	if(isTRUE(model.family %in% model.family.od))
# 		colnames(fit.tab)	<- c("deviance","df.residual","prob(>chi_sq)",paste("F vs.",model.design.reference,sep=""),paste("df vs.",model.design.reference,sep=""),"prob(>F)")
# 	if(isTRUE(model.family %in% model.family.other))
# 		colnames(fit.tab)	<- c("deviance","df.residual","prob(>chi_sq)",paste("LR vs.",model.design.reference,sep=""),paste("df vs.",model.design.reference,sep=""),"prob(>chi_sq)","aic")
	######################
	return(fit.tab)


	
}	#	apc.fit.table.2s


