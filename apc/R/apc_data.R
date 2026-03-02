#######################################################
#	apc package
#	Bent Nielsen,  7 Dec 2023, version 3.0.0
#	Bent Nielsen, 12 Apr 2016, version 1.2.1
#	Data list and Data examples
#######################################################
#	Copyright 2014-2023 Bent Nielsen
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

apc.data.list	<- function(response, data.format, dose=NULL, age1=NULL, per1=NULL, coh1=NULL, unit=NULL, per.zero=NULL, per.max=NULL, time.adjust=NULL, label=NULL, n.decimal=NULL, add.names.to.data.matrix=TRUE)
# BN 23 Jun 2025: Renamed from new.apc.data.list, Updated apc.data.list.Rd
#	BN 13 Dec 2023: Mixed frequency: Introduce APm, PAm data.format & change unit 
#	BN 11 Apr 2016: Change of default values
#	BN  1 Feb 2016: Added argument n.decimal
#	BN  8 Sep 2015:	Added argument label
#	BN 24 apr 2015
#	This function constructs list of apc.data.list type.
#	This gives the user a single focus for entering information about the data.
#	Only response and data.format are obligatory input.
#	Mixed frequency indicated through unit.  Only for data.format AP, PA 
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
#									"APm" 		as AP but with mixed frequency
#									"PAm" 		as PA but with mixed frequency
#			age1			smallest age    index (not used for data.format="CP", "PC")
#			per1			smallest period index (not used for data.format="AC","CA","CL","CL.vector.by.row","trapezoid")
#			coh1			smallest cohort index (not used for data.format="AP","PA")
#			unit			time units for indices.
#							default is 1
#							if unit is vector of length 2
#							then this is interpreted as mixed case
#								 arguments are then age and period arguments in that order.
#								 arguments must be integers and different
#								 data.format must be APm, PAm
#							if unit is vector of length 3
#							then this is interpreted as mixed case
#								 arguments are first common unit and then age and period arguments in that order.
#								 age and period must have greatest common divisor 1.
#								 data.format must be APm, PAm
#  			per.zero		Only used for data.format="trapezoid".
#									Entries in upper triangle with period <= per.zero are interpreted as NA.
#  			per.max 		Only used for data.format="trapezoid".
# 	 								Entries in lower triangle with period > per.zero+per.max are interpreted as NA.
#			time.adjust		Only two of age1, per1, coh1 are used.
#							The third is computed according to the formula
#							age1+coh1=per1+time.adjust
#			label			character.
#							particularly useful with multiple data sets
#			n.decimal		used for...?
#							default: set to 2 if unit has all elements less than 1 and larger than 1/20.
#			add.names.to.data.matrix
#							logical. Default TRUE
#							If True override names of response, dose matrices
#	out		list including all 12 arguments.
{	#	apc.data.list
	##############################
	#	15 Aug 2023
	#	greatest common divisor
	# from Matthew Lundberg on https://stackoverflow.com/questions/21502181/finding-the-gcd-without-looping-r/21504113#21504113
	function.gcd <- function(x, y) ifelse(y, Recall(y, x %% y), x)
	##############################
	#	check obligatory input
	data.format.list.regular<- c("AP","AC","CA","CL","CP","PA","PC","trap","trapezoid")
	data.format.list.mixed	<- c("APm","PAm")
	data.format.list		<- c(data.format.list.regular,data.format.list.mixed)
	data.format.list.matrix	<- c("AP","AC","CA","CL","CP","PA","PC","trap","trapezoid")	  		# is this used?
	if(isTRUE(data.format %in% data.format.list)==FALSE)
		stop("model.family has wrong argument")
	if(is.matrix(response)==FALSE)
		stop("response is not a matrix")
	#	check "CL" input
	if(data.format=="CL")
	{
		if(ncol(response) != nrow(response))	stop("apc.error: Response matrix is not square")
		k	<- nrow(response)
		for(age in 2:k)
			for(coh in (k+2-age):k)
				if(is.na(response[coh,age])==FALSE) stop("apc.error: Lower triangle of response matrix should be NA")
	}		
	##############################
	#	check unit input
	# 	Added 7 Dec 2023
	#	checking for regular or mixed case, where length of unit =1 or =2
	if(is.null(unit))	unit=1			# 23 Jun 2025 added
	if(is.vector(unit,mode="numeric")==FALSE)	stop("unit is not numeric vector")
	if(length(unit) > 3)						stop("unit should have length <=3")
	if(length(unit)== 2)						unit <- c(1,unit)		# 24 Jun 2025 revised
	if(length(unit)== 3){																# 24 Jun 2025 revised
		if(data.format %in% data.format.list.regular){
			if(unit[2]==unit[3]){
				warning("data.format is reguler (not mixed), but length(unit)>1. Only unit[1] is used.")
  			unit	<- c(unit[1]*unit[2],1,1)
			}
			else stop("data.format is regular (not mixed), but unit has different values for age and period.")
		}		
		if(data.format %in% data.format.list.mixed){
			if(unit[2]==unit[3]){
				warning("data.format is mixed, but unit has the same values for age and period. data.format changed to regular and only age value is used.")
  			if(data.format=="APm")	data.format	<- "AP"
	  		if(data.format=="PAm")	data.format	<- "PA"
  			unit	<- c(unit[1]*unit[2],1,1)
			}
			else{
				g		<- function.gcd(unit[1],unit[2])
				if(g!=1){
					warning(paste("data.format is mixed, but age & period units have greatest common divisor ",g," >1. Units for common scale, age & period changed from ",unit," to ",unit[1]*g,unit[2]/g,unit[3]/g))
				}	
				unit	<- c(g,unit[2:3]/g)   # BN 4 Jul 2025 corrected
			}
		}
	}
	# old from 7 Dec 2023	
#	if(length(unit)== 3){
#		if(data.format %in% data.format.list.regular){
#			if(isTRUE(unit[2]==1)==FALSE || isTRUE(unit[3]==1)==FALSE )
#				stop("unit[2] <>1 or unit[3] <>1 but data.format is not mixed. Only first element of unit is used")
#		}		
#		if(data.format %in% data.format.list.mixed && unit[2]==unit[3]){
#			warning("data.format is mixed, but unit[2]=unit[3].  data.format changed to regular and only unit[1]*unit[2] is used")
#			if(data.format=="APm")	data.format	<- "AP"
#			if(data.format=="PAm")	data.format	<- "PA"
#			unit	<- c(unit[1]*unit[2],1,1)
#		}
#	}	
#	if(length(unit)== 2){
#		if(data.format %in% data.format.list.regular){
#			if(unit[1]==unit[2]){
#				warning("data.format is not mixed.  Only unit[1] is used")
#				unit	<- c(unit[1],1,1)
#			}
#			else stop("data.format is not mixed but unit[1]<>unit[2]")
#		}	
#		if(data.format %in% data.format.list.mixed){
#			if(unit[1]==unit[2]){
#				warning("data.format is mixed, but unit[1]=unit[2].  data.format changed to regular and only unit[1] is used")
#				if(data.format=="APm")	data.format	<- "AP"
#				if(data.format=="PAm")	data.format	<- "PA"
#				unit	<- c(unit[1],1,1)
#			}
#			else{
#				g		<- function.gcd(unit[1],unit[2])
#				if(g!=1){
#					warning(paste("data.format is mixed, but has greatest common divisor ",g," >1. Unit changed from ",unit[1],unit[2]," to ",unit[1]/g,unit[2]/g))
#				}	
#				unit	<- c(g,unit/g)
#	}	}	}		
	if(length(unit)==1){
		if(data.format %in% data.format.list.regular)	unit	<- c(unit,1,1)	
		else stop("data.format is mixed but length(unit)=1")
	}	
	##############################
	# 	15 Aug 2023 
	#	set default values for time adjust
	if(is.null(time.adjust))
		if(is.null(age1)+is.null(per1)+is.null(coh1)>1)
			time.adjust <- unit[1]
		else	
			time.adjust <- 0

	##############################
	# 	15 Aug 2023 
	#	set default values for age1, per1, coh1
	if(data.format %in% data.format.list.regular)
	{
		if(is.null(age1))	age1	<- unit[1]
		if(is.null(per1))	per1	<- unit[1]
		if(is.null(coh1))	coh1	<- unit[1]
	}
	if(data.format %in% data.format.list.mixed)
	{
		if(is.null(age1))	age1	<- unit[1]*unit[2]
		if(is.null(per1))	per1	<- unit[1]*unit[3]							
	}
			
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
	function.get.dim.names <- function(m,x1,x.unit,x.interval,y1,y.unit,y.interval)
	{	#	function.get.dim.names
		function.one.set.of.names <- function(mm,dim1,dim.unit,interval)
		{	#	function.one.set.of.names
			dim.length	<- nrow(mm)
			if(interval && dim.unit>1)	{
				dim.names	<- paste(as.character(seq(from=dim1,length=dim.length,by=dim.unit)),
							 		 "-",
							 		 as.character(seq(from=dim1,length=dim.length,by=dim.unit)+dim.unit-1),
									 sep="")
				}									 
			else	{									 
				dim.names	<- as.character(seq(from=dim1,length=dim.length,by=dim.unit))
			}	
			return(dim.names)						 
		}	#	function.one.set.of.names
		if(is.null(rownames(m)) || add.names.to.data.matrix)	{
			rownames(m) <- function.one.set.of.names(  m ,x1,x.unit,x.interval) }
		if(is.null(colnames(m)) || add.names.to.data.matrix)	{
			colnames(m) <- function.one.set.of.names(t(m),y1,y.unit,y.interval) }
		return(m)
	}	#	function.get.dim.names
	# 13 Dec 2023: adapted for mixed case
		x.interval	<- y.interval	<- FALSE
		x.unit	<- y.unit	<- unit[1]
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
		if(data.format %in% c("APm"))
			{	x1 <- age1; y1 <- per1; x.unit <- x.unit*unit[2]; y.unit <- y.unit*unit[3];	x.interval <- TRUE	}	
		if(data.format %in% c("PAm"))
			{	x1 <- per1; y1 <- age1; x.unit <- x.unit*unit[3]; y.unit <- y.unit*unit[2]; y.interval <- TRUE	}	
	response 	<- function.get.dim.names(response,x1,x.unit,x.interval,y1,y.unit,y.interval)
	if(is.null(dose)==FALSE)
		dose	<- function.get.dim.names(dose,	   x1,x.unit,x.interval,y1,y.unit,y.interval)
	##############################
	#	n decimal
	#	15 Aug 2023: changed to allow for vector unit
	if(is.null(n.decimal))
		if(unit[1]<1 && unit[1]>=1/20)	n.decimal	<- 2
	##############################
	return(list(response	    =response	     ,
				dose		    =dose		     ,
				data.format	    =data.format     ,
				age1		    =age1		     ,
				per1		    =per1		     ,
				coh1		    =coh1		     ,
				unit		    =unit		     ,
				per.zero	    =per.zero	     ,
				per.max		    =per.max	     ,
				time.adjust	    =time.adjust     ,
				label		    =label		     ,
				n.decimal	    =n.decimal	     ))
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

