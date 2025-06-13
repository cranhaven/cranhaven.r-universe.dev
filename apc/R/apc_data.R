#######################################################
#	apc package
#	Bent Nielsen, 12 April 2016, version 1.2.1
#	Data list and Data examples
#######################################################
#	Copyright 2014, 2015 Bent Nielsen
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

apc.data.list	<- function(response, data.format, dose=NULL, age1=NULL, per1=NULL, coh1=NULL, unit=NULL, per.zero=NULL, per.max=NULL, time.adjust=NULL, label=NULL, n.decimal=NULL)
#	BN 11 April 2016: Change of default values
#	BN 1 Feb 2016: 	Added argument n.decimal
#	BN 8 Sep 2015:	Added argument label
#	BN 24 apr 2015
#	This function constructs list of apc.data.list type.
#	This gives the user a single focus for entering information about the data.
#	Only response and data.format are obligatory input.
#	in:		response		matrix of responses
#			dose			NULL or matrix of dose.
#			data.format		character indicating format of data.matrix
#									"AP"		has    age/period as increasing row/column index
#									"AC"		has    age/cohort as increasing row/column index
#									"CA"		has cohort/age    as increasing row/column index
#									"CL"		has cohort/age 	  as increasing row/column index, triangular
#									"CP"		has cohort/period as increasing row/column index
#									"PA"		has period/age    as increasing row/column index
#									"PC"		has period/cohort as increasing row/column index
#									"trapezoid"	has    age/period as increasing row/column index,
#													period-diagonals are NA for period <= per.zero and >per.zero+per.max 
#			age1			smallest age    index (not used for data.format="CP", "PC")
#			per1			smallest period index (not used for data.format="AC","CA","CL","CL.vector.by.row","trapezoid")
#			coh1			smallest cohort index (not used for data.format="AP","PA")
#			unit			time units for indices
#  			per.zero		Only used for data.format="trapezoid".
#									Entries in upper triangle with period <= per.zero are interpreted as NA.
#  			per.max 		Only used for data.format="trapezoid".
# 	 								Entries in lower triangle with period > per.zero+per.max are interpreted as NA.
#			time.adjust		Only two of age1, per1, coh1 are used.
#							The third is computed according to the formula
#							age1+coh1=per1+time.adjust
#			label			character.
#							particularly useful with multiple data sets
#	out		list including all 8 arguments.
{	#	apc.data.list
	##############################
	#	check obligatory input
	data.format.list		<- c("AP","AC","CA","CL","CP","PA","PC","trap","trapezoid")
	data.format.list.matrix	<- c("AP","AC","CA","CL","CP","PA","PC","trap","trapezoid")
	if(isTRUE(data.format %in% data.format.list)==FALSE)
		return(cat("apc.error: model.family has wrong argument \n"))
	if(is.matrix(response)==FALSE)
		return(cat("apc.error: response is not a matrix \n"))
	#	check "CL" input
	if(data.format=="CL")
	{
		if(ncol(response) != nrow(response))	return(cat("apc.error: Response matrix is not square \n"))
		k	<- nrow(response)
		for(age in 2:k)
			for(coh in (k+2-age):k)
				if(is.na(response[coh,age])==FALSE) return(cat("apc.error: Lower triangle of response matrix should be NA \n"))
	}		
	##############################
	#	set default values
	# 	Added 11 April 2016
	if(is.null(unit))			unit<- 1		
	if(is.null(age1)+is.null(per1)+is.null(coh1)>1)	   	
	{	if(is.null(time.adjust))	time.adjust <- unit		
	}
	else
	{	if(is.null(time.adjust))	time.adjust <- 0
	}
	if(is.null(age1))	age1	<- unit
	if(is.null(per1))	per1	<- unit
	if(is.null(coh1))	coh1	<- unit
		
	##############################
	#	set default values for trapezoids
	if(data.format=="trap")			data.format	<- "trapezoid"
	if(data.format=="trapezoid")
	{
		if(is.null(per.zero))	per.zero	<-0;
		if(is.null(per.max))	per.max	<-nrow(response)+ncol(response)-1-per.zero;
	}
	else
	{	per.zero	<- NULL;
		per.max	<- NULL;
	}
	##############################
	#	row and column names
	function.get.dim.names <- function(m,x1,x.unit,y1,y.unit)
	{	#	function.get.dim.names
		function.one.set.of.names <- function(mm,dim1,dim.unit)
		{	#	function.one.set.of.names
			dim.length	<- nrow(mm)
#			if(is.integer(dim.unit) && dim.unit>1)
#				dim.names	<- paste(as.character(seq(from=dim1,length=dim.length,by=dim.unit)),
#							 		 "-",
#							 		 as.character(seq(from=dim1,length=dim.length,by=dim.unit)+dim.unit-1),
#									 sep="")
#			else									 
				dim.names	<- as.character(seq(from=dim1,length=dim.length,by=dim.unit))
			return(dim.names)						 
		}	#	function.one.set.of.names
		if(is.null(rownames(m)))
			rownames(m) <- function.one.set.of.names(m,x1,x.unit)
		if(is.null(colnames(m)))
			colnames(m) <- function.one.set.of.names(t(m),y1,y.unit)
		return(m)
	}	#	function.get.dim.names
	if(data.format %in% c("AC","trap","trapezoid"))
		{	x1 <- age1; y1 <- coh1	}
	if(data.format %in% c("CA","CL"))
		{	x1 <- coh1; y1 <- age1	}	
	if(data.format %in% c("AP"))
		{	x1 <- age1; y1 <- per1	}	
	if(data.format %in% c("PA"))
		{	x1 <- per1; y1 <- age1	}	
	if(data.format %in% c("CP"))
		{	x1 <- coh1; y1 <- per1	}	
	if(data.format %in% c("PC"))
		{	x1 <- per1; y1 <- coh1	}	
	response 	<- function.get.dim.names(response,x1,unit,y1,unit)
	if(is.null(dose)==FALSE)
		dose	<- function.get.dim.names(dose,	x1,unit,y1,unit)
	##############################
	#	n decimal
	if(is.null(n.decimal))
		if(unit<1 && unit>=1/20)	n.decimal	<- 2
	##############################
	return(list(response	=response	,
				dose		=dose		,
				data.format	=data.format,
				age1		=age1		,
				per1		=per1		,
				coh1		=coh1		,
				unit		=unit		,
				per.zero	=per.zero	,
				per.max		=per.max	,
				time.adjust	=time.adjust,
				label		=label		,
				n.decimal	=n.decimal	))
}	#	apc.data.list

is.triangle	<- function(m)
#	BN 21 Nov 2019
#	checks if input is a matrix with "CL" form.
#	in:	m			matrix with "CL" format Dimension kxk.
#					Upper left triangle filled by v, row by row.
#					Remaining entries NA
#					in incremental form
{	#	is.triangle
	answer	<- TRUE
	if(!is.matrix(m))
	{	answer	<- FALSE
		warning("argument is not a matrix")	}
	if(answer==TRUE)	
	if(!(nrow(m)==ncol(m)))
	{	answer	<- FALSE
		warning("argument is not a square matrix")	}
	if(answer==TRUE)	
	for(row in 1:nrow(m))
		for(col in 1:(nrow(m)-row+1))
			if(is.na(m[row,col]))
			{	answer	<- FALSE
				warning("na.s in upper triangle") }
	if(answer==TRUE)	
	for(row in 2:nrow(m))
		for(col in (nrow(m)-row+2))
			if(!is.na(m[row,col]))
			{	answer	<- FALSE
				warning("numbers in lower triangle") }
	return(answer)
}	#	is.triangle

vector.2.triangle	<- function(v,k)
#	BN 7 Feb 2015
#	function to organise a vector as a triangle.
#	useful for reserving data
#	in:		v		vector. Length k*(k+1)/2
#			k		integer. Dimension	
#	out:	m		matrix with "CL" format Dimension kxk.
#					Upper left triangle filled by v, row by row.
#					Remaining entries NA
{	#	vector.2.triangle	
	##############################
	#	Check input
	if(is.vector(v)==FALSE)		return(cat("vector.2.triangle: v is not a vector \n"))
	if(length(v) != k*(k+1)/2)	return(cat("vector.2.triangle: Length of v does not match k\n"))
	##############################
	#	turn into matrix
	m	<- matrix(nrow=k,ncol=k,data=NA)
	i	<- 0
	for(coh in 1:k)
	{
		m[coh,1:(k+1-coh)]	<- v[(i+1):(i+k+1-coh)]
		i	<- i+k+1-coh
	}
	return(m)
}	#	vector.2.triangle	

triangle.cumulative	<- function(m)
#	BN 21 Nov 2019
#	cumulates an incremental triangle
#	in:	m			apc.data.list
#					OR
#					matrix with "CL" format Dimension kxk.
#					Upper left triangle filled by v, row by row.
#					Remaining entries NA
#					in incremental form
#	out: m.cum	 	matrix in "CL" format, cumulated
{	#	triangle.cumulative
	##############################
	#	extract response matrix if input is an apc.data.list
	m.m	<- m				
	if(is.list(m))	m.m 	<- m$response				
	##############################
	#	Check input
	if(!is.triangle(m.m))			warning("m is not a triangle matrix, tries anyway...")
	##############################
	m.cum	<- m.m
	for(col in 2:ncol(m.cum))
		m.cum[,col]	<- m.cum[,(col-1)] + m.cum[,col]
	return(m.cum)	
}	#	triangle.cumulative

triangle.incremental	<- function(m)
#	BN 21 Nov 2019
#	find increments of cumulative triangle
#	in:	m			apc.data.list
#					OR
#					matrix with "CL" format Dimension kxk.
#					Upper left triangle filled by v, row by row.
#					Remaining entries NA
#					in incremental form
#	out: m.cum	 	matrix in "CL" format, incremental
{	#	triangle.incremental
	##############################
	#	extract response matrix if input is an apc.data.list
	m.m	<- m				
	if(is.list(m))	m.m 	<- m$response				
	##############################
	#	Check input
	if(!is.triangle(m.m))			warning("m is not a triangle matrix, tries anyway...")
	##############################
	m.inc	<- m.m
	for(col in ncol(m.inc):2)
		m.inc[,col]	<- m.inc[,col] - m.inc[,(col-1)]
	return(m.inc)	
}	#	triangle.incremental

