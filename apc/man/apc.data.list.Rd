\name{apc.data.list}
\alias{apc.data.list}
\title{Arrange data as an apc.data.list}
\description{This is step 1 of the apc analysis.

The apc package is aimed at range of data types.  This analysis and labelling of parameters depends on
the choice data type.  In order to keep track of this choice the data first has to be arranged as an
apc.data.list.  The function purpose of this function is to aid the user in constructing a list with the right information.

Age period cohort analysis is used in two situations.  A dose-response situation, where both doses (exposure, risk set, cases) and
responses (counts of deaths, outcomes) are available.  And a response situation where only a response is available.  If
the aim is to directly model mortality ratios (counts of death divided by exposure) this will be thought of a response

The \code{apc.data.list} gives sufficient information for the further analysis. It is sufficient to store this information.
It has 2 obligatory arguments, which are a response matrix and a character indicating the data format.
It also has some further optional arguments, which have certain default values. 
Some times it may be convenient to add further arguments to the \code{apc.data.list}.  This will not affect the apc analysis.

\code{apc.data.list} generates default row and column names for the response and dose matrices when these are not
provided by the user.
}
\usage{apc.data.list(response, data.format, dose=NULL,
					age1=NULL, per1=NULL, coh1=NULL, unit=NULL,
					per.zero=NULL, per.max=NULL,
					time.adjust=NULL, label=NULL,
					n.decimal=NULL)}
\arguments{
	\item{response}{matrix (or vector).  Numbers of responses.  It should have a format matching \code{data.format}.
			Time should be increasing with the row/column index of the matrix.  For instance, consider a 10x5 matrix in "AP" format:
			Then the row index is for age, and it should be increasing in age. Thus, higher ages are further down the rows of the matrix.
			In the same way, the column index is for period.}
	\item{data.format}{character.  The following options are implemented:
  	\describe{
			\item{"AC"}{has    age/cohort as increasing row/column index.}
			\item{"AP"}{has    age/period as increasing row/column index.}
			\item{"CA"}{has cohort/age    as increasing row/column index.}
			\item{"CL"}{has cohort/age 	  as increasing row/column index, triangular.}
			\item{"CP"}{has cohort/period as increasing row/column index.}
			\item{"PA"}{has period/age    as increasing row/column index.}
			\item{"PC"}{has period/cohort as increasing row/column index.}
			\item{"trapezoid"}{has    age/period as increasing row/column index, period-diagonals are NA for period <= per.zero and >per.zero+per.max.}
		}}
 	\item{dose}{\emph{Optional}. matrix or NULL.  Numbers of doses.  It should have same format as \code{response}.}
    \item{age1}{\emph{Optional}. Numeric or NULL.  Time label for youngest age group.  Used if \code{data.format} is "AC", "AP", "CA", "CL", "PA", "trapezoid". If NULL default is unit.}
    \item{per1}{\emph{Optional}. Numeric or NULL.  Time label for oldest period group.  Used if \code{data.format} is "AP", "CP", "PA", "PC". If NULL default is unit.}
    \item{coh1}{\emph{Optional}. Numeric or NULL.  Time label for youngest age group.  Used if \code{data.format} is "AC", "CA", "CL", "CL.vector.by.row", "CP", "PC", "trapezoid". If NULL default is unit.}
    \item{unit}{\emph{Optional}. Numeric or NULL.  Common time steps for age, period and cohort.  For quarterly data use \code{1/4}.  For monthly data use \code{1/12}. If NULL default is 1.}
	\item{per.zero}{\emph{Optional}. Numeric or NULL.  Needed if data format is "trapezoid".}
	\item{per.max}{\emph{Optional}. Numeric or NULL.  Needed if data format is "trapezoid".}
	\item{time.adjust}{\emph{Optional}. Numeric.  Time labels are based on two of age1, per1 and coh1.  The third time label is computed according to the formula age1+coh1=per1+time.adjust.  Default is 0.  If age1=coh=1 it is natural to choose time.adjust=1.}
	\item{label}{\emph{Optional}. Character.  Useful when working with multiple data sets. Some internal functions use the first three characters of the label for identification of the two datasets.}
	\item{n.decimal}{\emph{Optional}.  Numeric or NULL.  The labels for parameters involves a date. This is found by converting a number into a character.  If the value is set to \code{d} package uses \code{\link{sprintf}}. If the value is set to \code{NULL} and \code{unit==1/4} for quarterly data or \code{unit==1/12} for monthly data or \code{1/20<=unit && unit<1} then package uses \code{sprintf}. If the value is set to \code{NULL}	and \code{1/20>unit || unit>=1}	then package uses \code{\link{as.character}}, which looks nice for integers, but can be messy otherwise.}
}
\value{
	  	\item{response}{matrix (or vector).  Numbers of responses.}
	 	\item{dose}{matrix (or NULL).  Numbers of doses.}
		\item{data.format}{character.}
	    \item{age1}{Numeric. Default is NULL.}		
	    \item{per1}{Numeric. Default is NULL.}		
	    \item{coh1}{Numeric. Default is NULL.}		
	    \item{unit}{Numeric.  Default is NULL. For monthly data one use \code{unit=1/12}.}		
		\item{per.zero}{Numeric.  If data.format is not "trapezoid" the value is NULL. If data.format is "trapezoid" the coordinate system is in age-cohort format and this value counts how many periods are cut off. The default is \code{per.zero=0}.}
		\item{per.max}{Numeric.  If data.format is not "trapezoid" the value is NULL. If data.format is "trapezoid" the coordinate system is in age-cohort format and this value counts how many periods are included in the data array.  The default is \code{per.max=nrow(response)+ncol(response)-1-per.zero}.}
		\item{time.adjust}{Numeric.  Default is NULL.}
		\item{label}{Character. Default of NULL.}
		\item{n.decimal}{Numeric or NULL.}
}
\details{
If the user does not set values for any of \code{age1}, \code{per1}, \code{coh1}, \code{unit} then the value is set to \code{unit}.

The user can set values of  \code{age1}, \code{per1}, \code{coh1} that are incongruent.  The functions only use two these that are relevant for the chosen
\code{data.format}.  Example: the \code{data.format} may be \code{"AC"} and the user sets
\code{age1}, \code{per1}, but \code{age1}, \code{coh1} are relevant for this data format.
The \code{apc.data.list} then sets \code{coh1=unit}, by default, while ignoring the value for \code{per1}.  Other commands such as
\code{\link{apc.data.list.subset}} or \code{\link{apc.fit.table}}, 
will internally, as default option, call the function
\code{\link{apc.get.index}}. That function will, in this example, set \code{per1} according to the values of \code{age1} and \code{coh1}.  

If the user does not set a value for \code{time.adjust} this is set equal to \code{unit} when the user does not specify at least two \code{age1}, \code{per1}, \code{coh1}.
Otherwise it is set to 0.
The former choice matches the values in the theory papers, where indices count 1,2,... to follow standard notation for row/column indices for matrices, so that age+coh=per+unit.
The latter choice seeks to match a real time scale the user sets according to age+coh=per.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 17 Nov 2016}
\references{
Kuang, D., Nielsen, B. and Nielsen, J.P. (2008a) Identification of the age-period-cohort model and the extended chain ladder model. Biometrika 95, 979-986. \emph{Download}: \href{https://doi.org/10.1093/biomet/asn026}{Article}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2007/w5/KuangNielsenNielsen07.pdf}{Nuffield DP}.

Nielsen, B. (2014) Deviance analysis of age-period-cohort models. \emph{Download}: \href{http://www.nuffield.ox.ac.uk/economics/papers/2014/apc_deviance.pdf}{Nuffield DP}.

Nielsen, B. (2015) apc: An R package for age-period-cohort analysis. \emph{R Journal} 7, 52-64. \emph{Download}: \href{https://journal.r-project.org/archive/2015-2/nielsen.pdf}{Open access}.
}
\seealso{The below example shows how the \code{\link{data.Japanese.breast.cancer}} data.list was generated.
Other provided data sets include
\code{\link{data.asbestos}}
\code{\link{data.Belgian.lung.cancer}}
\code{\link{data.Italian.bladder.cancer}}.

A subset of the data can be selected using \code{\link{apc.data.list.subset}}.
}
\examples{
###############
#	Artificial data
#	(1) Generate a 5x7 matrix and make arbitrary decisions for rest

response <- matrix(data=seq(1:35),nrow=5,ncol=7)
data.list	<- apc.data.list(response=response,data.format="AP",
					age1=25,per1=1955,coh1=NULL,unit=5,
					per.zero=NULL,per.max=NULL)
data.list

#	(2) Chain Ladder data

k			<- 5
v.response 	<- seq(1:(k*(k+1)/2))
data.list	<- apc.data.list(response=vector.2.triangle(v.response,k),
							data.format="CL.vector.by.row",age1=2001)
data.list

###############
#	Japanese breast cancer
#	This is the code used to generate the data.Japanese.breast.cancer
v.rates		<- c( 0.44, 0.38, 0.46, 0.55, 0.68,
			 	  1.69, 1.69, 1.75, 2.31, 2.52,
				  4.01, 3.90, 4.11, 4.44, 4.80,
				  6.59, 6.57, 6.81, 7.79, 8.27,
				  8.51, 9.61, 9.96,11.68,12.51,
				 10.49,10.80,12.36,14.59,16.56,
				 11.36,11.51,12.98,14.97,17.79,
				 12.03,10.67,12.67,14.46,16.42,
				 12.55,12.03,12.10,13.81,16.46,
				 15.81,13.87,12.65,14.00,15.60,
				 17.97,15.62,15.83,15.71,16.52)
v.cases		<- c(   88,   78,  101,  127,  179,
				   299,  330,  363,  509,  588,
				   596,  680,  798,  923, 1056,
				   874,  962, 1171, 1497, 1716,
				  1022, 1247, 1429, 1987, 2398,
				  1035, 1258, 1560, 2079, 2794,
				   970, 1087, 1446, 1828, 2465,
				   820,  861, 1126, 1549, 1962,
				   678,  738,  878, 1140, 1683,
				   640,  628,  656,  900, 1162,
				   497,  463,  536,  644,  865)				 
#	see also example below for generating labels

rates	<- matrix(data=v.rates,nrow=11, ncol=5,byrow=TRUE)
cases	<- matrix(data=v.cases,nrow=11, ncol=5,byrow=TRUE)

# 	A data list is now constructed as follows
#	note that list entry rates is redundant,
#	but included since it represents original data

data.Japanese.breast.cancer	<- apc.data.list(response=cases,
			dose=cases/rates,data.format="AP",
			age1=25,per1=1955,coh1=NULL,unit=5,
			per.zero=NULL,per.max=NULL,time.adjust=0,
			label="Japanese breast cancer")

#	or when exploiting the default values

data.Japanese.breast.cancer	<- apc.data.list(response=cases,
			dose=cases/rates,data.format="AP",
			age1=25,per1=1955,unit=5,
			label="Japanese breast cancer")

###################################################
# 	Code for generating labels

row.names <- paste(as.character(seq(25,75,by=5)),"-",as.character(seq(29,79,by=5)),sep="")
col.names <- paste(as.character(seq(1955,1975,by=5)),"-",as.character(seq(1959,1979,by=5)),sep="")

}