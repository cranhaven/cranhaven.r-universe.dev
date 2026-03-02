\name{data.loss.TA}
\alias{data.loss.TA}
\title{Motor data}
\description{
Function that organises loss data in \code{\link{apc.data.list}} format.

The data set is taken from Table 1 of Verrall (1991), who 
attributes the data to Taylor and Ashe (1983).
It includes a run-off triangle:
"response" (X) is paid amounts (units not reported).

Data also analysed in various papers, e.g.
England and Verrall (1999).

The data set is in "CL"-format.

At present \code{apc.package} does not have functions for either forecasting or for exploiting the counts.
For this one can with advantage use the \code{DCL.package}.
}
\usage{data.loss.TA}
\value{The value is a list in \code{\link{apc.data.list}} format.
	\item{response}{vector of paid amounts, X}
	\item{dose}{NULL.}
	\item{data.format}{logical. Equal to "CL.vector.by.row".  Data organised in vectors.}
	\item{age1}{numeric. Equal to 1.}
	\item{per1}{NULL. Not needed when data.format="CL"}
	\item{coh1}{numeric. Equal to 1.}
	\item{unit}{numeric. Equal to 1.}
	\item{per.zero}{NULL.  Not needed when data.format="CL"} 
	\item{per.max}{NULL.  Not needed when data.format="CL"} 
	\item{time.adjust}{0.  Thus age=1 in cohort=1 corresponds to period=1+1-1+0=1.}	
	\item{label}{character. "loss TA".}						
}
\source{Tables 1 of Verrall (1991).}
\references{                                                                         
England, P., Verrall, R.J. (1999)                                        
Analytic and bootstrap estimates of prediction errors in claims reserving
Insurance: Mathematics and Economics 25, 281-293                         

Taylor, G.C., Ashe, F.R. (1983)
Second moments of estimates of outstanding claims
Journal of Econometrics 23, 37-61

Verrall, R.J. (1991)                                                     
On the estimation of reserves from loglinear models                      
Insurance: Mathematics and Economics 10, 75-80                           
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 8 Sep 2015 (18 Mar 2015)}
\seealso{General description of \code{\link{apc.data.list}} format.}
\examples{
#########################
##	It is convient to construct a data variable

data	<- data.loss.TA()

##	To see the content of the data

data

#########################
#	Fit chain-ladder model

apc.fit.table(data,"poisson.response")

#	The overdispersed poisson model is experimental at the moment,
#	so not documented
apc.fit.table(data,"od.poisson.response")


}
