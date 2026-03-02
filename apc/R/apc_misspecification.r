#######################################################
#	apc package
#	Bent Nielsen, 17 Jan 2024, version 3.0.0
#	functions to test for misspecification
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

apc.test.normal.residuals <- function(list_or_vector,remove.zeros=FALSE,tolerance=1e-10)
#	input:	list_or_vector		EITHER	list with object residuals
#								OR		vector of residuals
#			
{	#	test_normal_residuals
	#	get residuals
	if(is.list(list_or_vector))
	{	if(exists("residuals", where=list_or_vector))
		{	v.residuals	<- list_or_vector$residuals
			if(is.vector(v.residuals)==FALSE)
				warning("Argument is list with object residuals, but object is not a vector")	
		}	else
			warning("Argument is list but has no object called 'residuals'")
	}	else
		{	if(is.vector(list_or_vector))
			{	v.residuals	<- list_or_vector
			}	else
				warning("Argument is neither list nor vector")
		}												
#	if(is.null(fit))	v.residuals	<- residuals
#	else				v.residuals	<- fit$residuals
	#	get number of obs
	l.n		<- length(v.residuals)
	#	remove zeros
	if(remove.zeros)
	{	l.n.zero	<- sum(abs(v.residuals)<1e-10)
		warning(paste("removed ",l.n.zero," zeros out of ",l.n," observations with tolerance ",tolerance))
		v.residuals	<- v.residuals[order(abs(v.residuals))[(l.n.zero+1):l.n]]
		l.n		<- length(v.residuals)
	}
	#	get mean
	l.m1	<- sum(v.residuals)/l.n
	l.v.res.demean	= v.residuals -	l.m1
	#	central moments
	l.c2	<- sum(l.v.res.demean^2)/l.n
	l.c3	<- sum(l.v.res.demean^3)/l.n
	l.c4	<- sum(l.v.res.demean^4)/l.n
	#	cumulants
	l.k3	<- l.c3/(l.c2^(3/2))
	l.k4	<- l.c4/(l.c2^2)-3
	#	test statistics
	l.t3	<- l.n*(l.k3)^2/6
	l.t4	<- l.n*(l.k4)^2/24
	l.t34	<- l.t3+l.t4
	#	p values
	l.p.t3	<- pchisq(l.t3 ,1,lower.tail=FALSE)
	l.p.t4	<- pchisq(l.t4 ,1,lower.tail=FALSE)
	l.p.t34	<- pchisq(l.t34,2,lower.tail=FALSE)
	#	table
	table_normal_residuals	<- matrix(NA, nrow=6, ncol =4)
	dimnames(table_normal_residuals)	<- list(c("obs","mean","variance","skewness","kurtosis","skew+kurt"),c("cumulant","test stat","df","p-value"))
	table_normal_residuals[2:5,1]	<- c(l.m1,l.c2,l.k3,l.k4)
	table_normal_residuals[4:6,2]	<- c(l.t3,l.t4,l.t34)
	table_normal_residuals[c(1,4:6),3]	<- c(l.n,1,1,2)
	table_normal_residuals[4:6,4]	<- pchisq(table_normal_residuals[4:6,2],table_normal_residuals[4:6,3],lower.tail=FALSE)
 	#################
	#	return
	return(table_normal_residuals)
}	#	test_normal_residuals
