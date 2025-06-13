#######################################################
#	apc package
#	Bent Nielsen, 7 May 2017, version 1.3.1
#	function to get indices of data and to generate sub sets of data
#######################################################
#	Copyright 2014-2017 Bent Nielsen
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

apc.get.index	<- function(apc.data.list)
#	BN 11 Apr 2016: check age1, per1, coh1
#	BN 4 Jan 2016:  check values added.
#					change coh1 for "AP" and "PA"	
#	BN 6 Feb 2015
#	function to get indices to keep track of estimation and labelling
#	in:		apc.data.list
#	out:	list 
{	#	apc.get.index
	#########################
	#	get values
	response	<- apc.data.list$response
	dose		<- apc.data.list$dose
	data.format	<- apc.data.list$data.format	
	age1		<- apc.data.list$age1		
	per1		<- apc.data.list$per1		
	coh1		<- apc.data.list$coh1		
	unit		<- apc.data.list$unit		
	per.zero	<- apc.data.list$per.zero	
	per.max		<- apc.data.list$per.max
	time.adjust	<- apc.data.list$time.adjust
	n.decimal	<- apc.data.list$n.decimal
	#########################
	#	check values - 3 Jan 2016
	if(is.null(time.adjust))	time.adjust <- 0
	#########################
	#	data.format SPECIFIC CODE
	#########################
	if(data.format=="AC")
	#	matrix with age/cohort as increasing row/column index
	#	NA: none
	{	nrow		<- nrow(response)
		ncol		<- ncol(response)
		n.data		<- nrow*ncol
		age.max		<- nrow
		coh.max		<- ncol
		per.max		<- age.max+coh.max-1
		per.zero	<- 0
		age1		<- age1
		coh1		<- coh1
		per1		<- age1+coh1-time.adjust
		index.data	<- matrix(nrow=n.data,ncol=2)
		col	<- 0
		for(row in 1:nrow)
		{
			index.data[(col+1):(col+ncol),1]	<- row		  							#	age
			index.data[(col+1):(col+ncol),2]	<- seq(1,ncol)							#	cohort
			col	<- col + ncol
		}
		index.trap	<- index.data
		data.xlab	<- "age"
		data.ylab	<- "cohort"
		data.x1	<- age1
		data.y1	<- coh1
	}
	#########################
	if(data.format=="AP")
	#	matrix with age/period as increasing row/column index
	#	NA: none
	{	nrow		<- nrow(response)
		ncol		<- ncol(response)
		n.data		<- nrow*ncol
		age.max		<- nrow
		per.max		<- ncol
		coh.max		<- age.max+per.max-1
		per.zero	<- age.max-1
		age1		<- age1
		per1		<- per1
		coh1		<- per1 - (per.zero*unit+age1)+time.adjust							# 16 Apr 2016: corrected
		index.data	<- matrix(nrow=n.data,ncol=2)
		index.trap	<- matrix(nrow=n.data,ncol=2)
		col	<- 0
		for(row in 1:nrow)
		{
			index.data[(col+1):(col+ncol),1]	<- row		  							#	age
			index.data[(col+1):(col+ncol),2]	<- seq(1,ncol)							#	period
			index.trap[(col+1):(col+ncol),1]	<- row									#	age
			index.trap[(col+1):(col+ncol),2]	<- seq((nrow-row+1),(nrow-row+ncol))	#	cohort
			col	<- col + ncol
		}
		data.xlab	<- "age"
		data.ylab	<- "period"
		data.x1	<- age1
		data.y1	<- per1
	}
	#########################
	if(data.format=="CA")
	#	matrix with cohort/age as increasing row/column index
	#	NA: none
	{	nrow		<- nrow(response)
		ncol		<- ncol(response)
		n.data		<- nrow*ncol
		age.max		<- ncol
		coh.max		<- nrow
		per.max		<- age.max+coh.max-1
		per.zero	<- 0
		age1		<- age1
		coh1		<- coh1
		per1		<- age1+coh1-time.adjust
		index.data	<- matrix(nrow=n.data,ncol=2)
		index.trap	<- matrix(nrow=n.data,ncol=2)
		col	<- 0
		for(row in 1:nrow)
		{
			index.data[(col+1):(col+ncol),1]	<- row		  							#	age
			index.data[(col+1):(col+ncol),2]	<- seq(1,ncol)							#	cohort
			index.trap[(col+1):(col+ncol),1]	<- seq(1,ncol)							#	cohort 
			index.trap[(col+1):(col+ncol),2]	<- row		  							#	age    
			col	<- col + ncol
		}
		data.xlab	<- "cohort"
		data.ylab	<- "age"
		data.x1	<- coh1
		data.y1	<- age1
	}
	#########################
	if(data.format=="CL")
	#	square matrix with cohort/age as increasing row/column index
	#	NA: in botton right triangle so period >= age.max+period.max
	{	k	<- min(nrow(response),ncol(response))
		##############################
		#	check obligatory input
		if(ncol(response) != nrow(response))	return(cat("apc.get.index error: Response matrix is not square \n"))
		for(age in 2:k)
			for(coh in (k+2-age):k)
				if(is.na(response[coh,age])==FALSE) return(cat("apc.get.index error: Lower triangle of response matrix should be NA \n"))
		##############################
		nrow		<- k
		ncol		<- k
		n.data		<- k*(k+1)/2
		age.max		<- k
		coh.max		<- k
		per.max		<- k
		per.zero	<- 0
		age1		<- age1
		coh1		<- coh1
		per1		<- age1+coh1-time.adjust
		index.data	<- matrix(nrow=n.data,ncol=2)
		index.trap	<- matrix(nrow=n.data,ncol=2)
		col	<- 0
		for(row in 1:nrow)
		{
			index.data[(col+1):(col+k+1-row),1]	<- row		  							#	cohort
			index.data[(col+1):(col+k+1-row),2]	<- seq(1,k+1-row)						#	age   
			index.trap[(col+1):(col+k+1-row),1]	<- seq(1,k+1-row)						#	age    
			index.trap[(col+1):(col+k+1-row),2]	<- row		  							#	cohort 
			col	<- col +k+1-row
		}
		data.xlab	<- "underwriting time (cohort)"
		data.ylab	<- "development time (age)"
		data.x1	<- coh1
		data.y1	<- age1
	}
	#########################
	if(data.format=="CP")
	#	matrix with cohort/period as increasing row/column index
	#	NA: none
	{	nrow		<- nrow(response)
		ncol		<- ncol(response)
		n.data		<- nrow*ncol
		coh.max		<- nrow
		per.max		<- ncol
		age.max		<- coh.max+per.max-1
		per.zero	<- coh.max-1
		per1		<- per1
		coh1		<- coh1
		age1		<- per1 - (per.zero*unit+coh1)+time.adjust
		index.data	<- matrix(nrow=n.data,ncol=2)
		index.trap	<- matrix(nrow=n.data,ncol=2)
		col	<- 0
		for(row in 1:nrow)
		{
			index.data[(col+1):(col+ncol),1]	<- row		  							#	cohort
			index.data[(col+1):(col+ncol),2]	<- seq(1,ncol)							#	period
			index.trap[(col+1):(col+ncol),1]	<- seq((nrow-row+1),(nrow-row+ncol))	#	age
			index.trap[(col+1):(col+ncol),2]	<- row									#	cohort
			col	<- col + ncol
		}
		data.xlab	<- "cohort"
		data.ylab	<- "period"
		data.x1	<- coh1
		data.y1	<- per1
	}
	#########################
	if(data.format=="PA")
	#	matrix with period/age as increasing row/column index
	#	NA: none
	{	nrow		<- nrow(response)
		ncol		<- ncol(response)
		n.data		<- nrow*ncol
		age.max		<- ncol
		per.max		<- nrow
		coh.max		<- age.max+per.max-1
		per.zero	<- age.max-1
		age1		<- age1
		per1		<- per1
		coh1		<- per1 - (per.zero*unit+age1)+time.adjust							# 16 Apr 2016: corrected
		index.data	<- matrix(nrow=n.data,ncol=2)
		index.trap	<- matrix(nrow=n.data,ncol=2)
		row	<- 0
		for(col in 1:ncol)
		{
			index.data[(row+1):(row+nrow),1]	<- seq(1,nrow)		  					#	period         
			index.data[(row+1):(row+nrow),2]	<- col									#	age            
			index.trap[(row+1):(row+nrow),1]	<- col									#	age
			index.trap[(row+1):(row+nrow),2]	<- seq((ncol-col+1),(ncol-col+nrow))	#	cohort
			row	<- row + nrow
		}
		data.xlab	<- "period"
		data.ylab	<- "age"
		data.x1	<- per1
		data.y1	<- age1
	}
	#########################
	if(data.format=="PC")
	#	matrix with period/cohort as increasing row/column index
	#	NA: none
	{	nrow		<- nrow(response)
		ncol		<- ncol(response)
		n.data		<- nrow*ncol
		coh.max		<- ncol
		per.max		<- nrow
		age.max		<- coh.max+per.max-1
		per.zero	<- coh.max-1
		per1		<- per1
		coh1		<- coh1
		age1		<- per1 - (per.zero*unit+coh1)+time.adjust
		index.data	<- matrix(nrow=n.data,ncol=2)
		index.trap	<- matrix(nrow=n.data,ncol=2)
		row	<- 0
		for(col in 1:ncol)
		{
			index.data[(row+1):(row+nrow),1]	<- seq(1,nrow)							#	period
			index.data[(row+1):(row+nrow),2]	<- col									#	cohort
			index.trap[(row+1):(row+nrow),1]	<- seq((ncol-col+1),(ncol-col+nrow))	#	age
			index.trap[(row+1):(row+nrow),2]	<- col									#	cohort
			row	<- row + nrow
		}
		data.xlab	<- "period"
		data.ylab	<- "cohort"
		data.x1	<- per1
		data.y1	<- coh1
	}
	#########################
	if(data.format=="trapezoid")
	#	trapezoid matrix with age/cohort as increasing row/column index
	#	NA: none
	{	nrow		<- nrow(response)
		ncol		<- ncol(response)
		age.max		<- nrow
		per.max		<- per.max
		coh.max		<- ncol
		per.zero	<- per.zero
		dim.lower	<- coh.max + age.max - 1 -per.zero - per.max		#	dimension of lower right triangle
		n.data		<- nrow*ncol - per.zero*(per.zero+1)/2 - dim.lower*(dim.lower+1)/2
		age1		<- age1
		coh1		<- coh1
		per1		<- age1+coh1+per.zero*unit-time.adjust				#	Changed 10 April 2016
		index.data	<- matrix(nrow=n.data,ncol=2)
		col	<- 0
		for(age in 1:age.max)
		{
			col.zero	<- max(per.zero-age+1,0)
			col.max		<- min(coh.max,per.zero+per.max+1-age)
			index.data[ (col+1):(col+col.max-col.zero),1]	<- age					    #	age
			index.data[ (col+1):(col+col.max-col.zero),2]	<- seq(col.zero+1,col.max)	#	cohort
			col	<- col + col.max-col.zero
		}
		index.trap	<- index.data
		data.xlab	<- "age"
		data.ylab	<- "cohort"
		data.x1	<- age1
		data.y1	<- coh1
	}
	#########################
	#	GENERAL CODE
	#########################
	#	get anchoring
	if(per.zero %% 2==0)	{	U <- (per.zero+2)%/% 2;	per.odd <- FALSE;	} 
	else					{	U <- (per.zero+3)%/% 2;	per.odd <- TRUE; 	} 
	########################
	return(list(response	=response	,	 #	argument
				dose		=dose		,	 #	argument
				data.format	=data.format,	 #	argument
				unit		=unit		,	 #	argument
				data.xmax	=nrow		,
				data.ymax	=ncol		,
				data.xlab	=data.xlab	,
				data.ylab	=data.ylab 	,
				data.x1		=data.x1	,
				data.y1		=data.y1	,
				n.data		=n.data		,
				index.data	=index.data	,
				index.trap	=index.trap	,
				age.max		=age.max	,
				per.max		=per.max	,
				coh.max		=coh.max	,
				per.zero	=per.zero	,
				per.odd		=per.odd	,
				U			=U			,
				age1		=age1		,
				per1		=per1		,
				coh1		=coh1	 	,
				n.decimal	=n.decimal	))
}	#	apc.get.index

apc.data.list.subset <- function (apc.data.list, age.cut.lower = 0, age.cut.upper = 0, 
											     per.cut.lower = 0, per.cut.upper = 0,
											     coh.cut.lower = 0, coh.cut.upper = 0,
											     apc.index = NULL, suppress.warning = FALSE) 
#	BN		24 apr 2017:	new code provided.
#							Jonas Harnau found bug in old code
#							& suggested completely new code
#							This code is improved here. 
#							The new code replaces code in version 1.3 from 1 Dec 2016 and earlier
#	function to get subset of data set
#	in:		apc.data.list
#	out:	list 
#	note:	if apc.index supplied then it suffices to input
#			apc.data.list = list(response=response,data.format=data.format,dose=dose)
#				where dose could be NULL
#			apc.index does not need to be a full apc.index list. Sufficient entries are
#						age.max
#						per.max
#						coh.max
#						index.trap
#						index.data
#						per.zero
#	note2: 	in code:
#			.cut refers to subset in old coordinate system
#			.new refers to subset in new coordinate system
{	#	apc.data.list.subset
	######################
	#	get index
  	if(is.null(apc.index)) apc.index <- apc.get.index(apc.data.list)
	##############################
	#	get index values, that are used
	age.max		<- apc.index$age.max				
	per.max		<- apc.index$per.max				
	coh.max		<- apc.index$coh.max
	age1		<- apc.index$age1    
	per1		<- apc.index$per1    
	coh1		<- apc.index$coh1    
	unit		<- apc.index$unit
	per.zero	<- apc.index$per.zero
	index.data	<- apc.index$index.data
	index.trap	<- apc.index$index.trap
	##############################
	#	get data.list values, that are used
	response	<- apc.data.list$response
	dose		<- apc.data.list$dose
	##############################
	#	warnings
	if(age.cut.lower>age.max-3 & !suppress.warning)	warning("age.cut.lower >= age.dim-2")
	if(per.cut.lower>per.max-3 & !suppress.warning)	warning("per.cut.lower >= per.dim-2")
	if(coh.cut.lower>coh.max-3 & !suppress.warning)	warning("coh.cut.lower >= coh.dim-2")
	if(age.cut.upper>age.max-3 & !suppress.warning)	warning("age.cut.upper >= age.dim-2")
	if(per.cut.upper>per.max-3 & !suppress.warning)	warning("per.cut.upper >= per.dim-2")
	if(coh.cut.upper>coh.max-3 & !suppress.warning)	warning("coh.cut.upper >= coh.dim-2")
	##############################
	#	errors
	if(age.cut.lower>=age.max)	stop("age.cut.lower >= age.dim")
	if(per.cut.lower>=per.max)	stop("per.cut.lower >= per.dim")
	if(coh.cut.lower>=coh.max)	stop("coh.cut.lower >= coh.dim")
	if(age.cut.upper>=age.max)	stop("age.cut.upper >= age.dim")
	if(per.cut.upper>=per.max)	stop("per.cut.upper >= per.dim")
	if(coh.cut.upper>=coh.max)	stop("coh.cut.upper >= coh.dim")
	##############################
	#	paste per=age+coh-1 column onto index.trap
	index.trap	<-	cbind(index.trap, apply(index.trap,1,sum)-1)
	##############################
	#	form binary vector with 1 if constraints are satisfied
	select	<-	( index.trap[,1] >  age.cut.lower                    
				& index.trap[,1] <= age.max - age.cut.upper
				& index.trap[,2] >  coh.cut.lower                    
				& index.trap[,2] <= coh.max - coh.cut.upper
				& index.trap[,3] >  per.cut.lower + per.zero                    
				& index.trap[,3] <= per.max - per.cut.upper	+ per.zero
				)
#  if(nrow(index.match.cut) == 0) stop("Cuts produce empty data set.", call. = TRUE)
	##############################
  	#	new definitions of parameters of index array
	age.min.cut	<- min(index.trap[select,1])
	per.min.cut	<- min(index.trap[select,3])
	coh.min.cut	<- min(index.trap[select,2])
	age.max.cut	<- max(index.trap[select,1]) #-age.min.cut+1
	per.max.cut	<- max(index.trap[select,3]) #-per.min.cut+1
	coh.max.cut	<- max(index.trap[select,2]) #-coh.min.cut+1
	#	within new coordinate system
	age.max.new	<- age.max.cut-age.min.cut+1	
	per.max.new	<- per.max.cut-per.min.cut+1	
	coh.max.new	<- coh.max.cut-coh.min.cut+1
	per.zero.new<- max(0, per.min.cut -1 - age.min.cut - coh.min.cut + 2)
	age1.new <-age1 + (age.min.cut -1) * unit
   	per1.new <-per1 + (per.min.cut -1) * unit
  	coh1.new <-coh1 + (coh.min.cut -1) * unit
	##############################
	#	adjust coordinates in index.trap
	index.trap[,1]	<- index.trap[,1] - age.min.cut + 1
	index.trap[,2]	<- index.trap[,2] - coh.min.cut + 1
	##############################
  	#	new data
	response.new	<- matrix(NA,nrow=age.max.new,ncol=coh.max.new)
	response.new[index.trap[select,c(1,2)]]	<- response[index.data[select,]]
	if(!is.null(dose)){
		dose.new	<- matrix(NA,nrow=age.max.new,ncol=coh.max.new)
		dose.new[index.trap[select,c(1,2)]]	<- dose[index.data[select,]]
	}	else dose.new = NULL
	##############################
	#	warnings
	if(!suppress.warning){
		#	subset becomes too small
		cut.old	<- c(age.cut.lower,age.cut.upper,
					 per.cut.lower,per.cut.upper,
					 coh.cut.lower,coh.cut.upper)
		cut.new <- c(age.min.cut-1,age.max-age.max.cut,
					 per.min.cut-1-per.zero,per.max-per.max.cut+per.zero,
					 coh.min.cut-1,coh.max-coh.max.cut)
		if(sum(cut.new)>sum(cut.old)){
#			print(sum(cut.old)>sum(pmax(0,cut.new)))
			cat("WARNING apc.data.list.subset: ")
			cat("cuts in arguments are:\n")
			print(cut.old)
			cat("have been modified to:\n")
			print(cut.new)
		}	
		#	data format is changed
		data.format.list.warning <- c("AP", "CA", "CL", "CP", "PA", "PC")
	  	if(apc.data.list$data.format %in% data.format.list.warning){
			cat("WARNING apc.data.list.subset: ")
			cat('coordinates changed to "AC" & data.format changed to "trapezoid"\n')
		}	
	}
	##############################
	#	warnings
	#	age.max.cut: largest age value
	#	age.min.cut: smallest age value
	if(age.max.cut-age.min.cut<=1 & !suppress.warning)	warning("age dimension <= 2")
	if(per.max.cut-per.min.cut<=1 & !suppress.warning)	warning("per dimension <= 2")
	if(coh.max.cut-coh.min.cut<=1 & !suppress.warning)	warning("coh dimension <= 2")
	##############################
	return(list(response	=response.new				,
				dose		=dose.new					,
				data.format	="trapezoid"				,
				age1		=age1.new					,
				per1		=per1.new					,
				coh1		=coh1.new					,
				unit		=apc.data.list$unit			,
				per.zero	=per.zero.new				,
				per.max		=per.max.new				,
				time.adjust	=apc.data.list$time.adjust	,
				label		=apc.data.list$label		,
				n.decimal	=apc.data.list$n.decimal	))
}	#	apc.data.list.subset

			