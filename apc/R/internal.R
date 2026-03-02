#######################################################
#	apc package
#	Bent Nielsen, 25 Jun 2025, version 3.0.0
#	Bent Nielsen, 1 Feb 2016, version 1.2
#	Internal functions
#######################################################
#	Copyright 2016 Bent Nielsen
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

apc.internal.function.date.2.character <- function(x,n.decimal=NULL)
{	#	apc.internal.function.date.2.character
	if(is.null(n.decimal))	return(as.character(x))
	if(n.decimal==1)		return(sprintf("%.1f",x))
	if(n.decimal==2)		return(sprintf("%.2f",x))
	if(n.decimal==3)		return(sprintf("%.3f",x))
	if(n.decimal==4)		return(sprintf("%.4f",x))
	if(n.decimal==5)		return(sprintf("%.5f",x))
	if(n.decimal==6)		return(sprintf("%.6f",x))
	if(n.decimal>6)			return(as.character(x))	
}	#	apc.internal.function.date.2.character

apc.internal.function.detrend	<- function(n)
#	BN, 3 apr 2015
#	in:		n			is the dimension
#	Out		m			matrix of dimension n x n
#						for detrending an n vector,
#						takes an identity matrix
#						replaces first column with (col-n)/(n-1)
#						replaces  last column with (1-col)/(n-1)
{	#	function.detrend
	#	m defines the detrending
	m			<- diag(n);
	m[1:n,1]	<- (seq(1:n)-n)/(n-1);
	m[1:n,n]	<- (1-seq(1:n))/(n-1);
	m[1,1]		<- 0;
	m[n,n]		<- 0;
	return(m)
}	#	function.detrend

function.coin	<- function(G,H)
#	BN 15 Aug 2023: finds properties of Frobenius coin problem
#	note algorithm is not optimized for large G,H		
#	in:		G,H		positive integers
{	#	coin
	l.coin	<- FALSE	
	if((G>1) && (H>1))	{	l.coin	<- TRUE	}
	#	Frobenius number.  Positive if G,H>1.  Otherwise -1
	F.GH	<- G*H-G-H
	#	Sylvester number.  Positive if G,H>1.  Otherwise 0
	S.GH	<- (G-1)*(H-1)/2
	#	Representable and Non-Representable numbers
	R.GH	<- N.GH	<- NULL
	if(l.coin)
	{
		i.upper	<- floor((F.GH-1)/G)
		for(i in 0:i.upper)
		{	j.upper	<- floor((F.GH-1-i*G)/H)
			if(j.upper>-1)
			{	for(j in 0:j.upper)
				{	R.GH	= c(R.GH,i*G+j*H)
		}	}	}
		N.GH	<- setdiff(1:F.GH,R.GH) 
	}
	########################
	return(list(Frobenius			= F.GH	,
				Sylvester			= S.GH	,
				non.representable	= N.GH	))
}	#	coin

function.coin.array	<- function(G,H,r.g,r.h)
#	BN 8 Dec 2023:
#	finds an array of all possible amounts that can be paid
#	with r.g G-coins and r.h H-coins.
#	in:		G,H,r.g,r.h		positive integers
#	out:					r.g x r.h matrix if r.g*r.h>0
# 							r.g vector		 if r.g>0 and r.h=0
# 							r.h vector		 if r.h>0 and r.g=0
#							null			 if r.g=r.h=0
{	#	function.coin.array
	if(r.g*r.h>0)	{
		m	<-   matrix(data=(1:r.g)*G,nrow=r.g,ncol=r.h,byrow=FALSE)
		m	<- m+matrix(data=(1:r.h)*H,nrow=r.g,ncol=r.h,byrow=TRUE)
	}
	if(r.g>0  && r.h==0)	{	m	<- (1:r.g)*G	}
	if(r.g==0 && r.h>0 )	{	m	<- (1:r.h)*H	}
	if(r.g==0 && r.h==0)	{	m	<- NULL			}
	########################
	return(m)
}	#	function.coin.array

