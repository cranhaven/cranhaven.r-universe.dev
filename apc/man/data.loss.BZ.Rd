\name{data.loss.BZ}
\alias{data.loss.BZ}
\title{Motor data}
\description{
Function that organises loss data in \code{\link{apc.data.list}} format.

The data set is taken from table 3.5 of Barnett & Zehnwirth (2000).
Source of data unclear.
It includes a run-off triangle:
"response" (X) is paid amounts (units not reported)
along with measures of exposure.

Data also analysed in e.g.
Kuang, Nielsen, Nielsen (2011).

The data set is in "CL"-format.

At present \code{apc.package} does not have functions for either forecasting or for exploiting the counts.
For this one can with advantage use the \code{DCL.package}.
}
\usage{data.loss.BZ}
\value{The value is a list in \code{\link{apc.data.list}} format.
	\item{response}{vector of paid amounts, X}
	\item{counts}{vector of number of reported claims, N}
	\item{dose}{NULL.}
	\item{data.format}{logical. Equal to "CL.vector.by.row".  Data organised in vectors.}
	\item{age1}{numeric. Equal to 1.}
	\item{per1}{NULL. Not needed when data.format="CL"}
	\item{coh1}{numeric. Equal to 1.}
	\item{unit}{numeric. Equal to 1.}
	\item{per.zero}{NULL.  Not needed when data.format="CL"} 
	\item{per.max}{NULL.  Not needed when data.format="CL"} 
	\item{time.adjust}{0.  Thus age=1 in cohort=1 corresponds to period=1+1-1+0=1.}	
	\item{label}{character. "loss BZ".}						
}
\source{Tables 1,2 of Verrall, Nielsen and Jessen (2010).}
\references{
Barnett G, Zehnwirth B (2000)
Best estimates for reserves.
Proc. Casualty Actuar. Soc. 87, 245--321.                         

Kuang D, Nielsen B, Nielsen JP (2011)
Forecasting in an extended chain-ladder-type model
\emph{Journal of Risk and Insurance} 78, 345-359         
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 8 Sep 2015 (18 Mar 2015)}
\seealso{General description of \code{\link{apc.data.list}} format.}
\examples{
#########################
##	It is convient to construct a data variable

data	<- data.loss.BZ()

##	To see the content of the data

data

#########################
#	Fit geometric chain-ladder model

apc.fit.table(data,"log.normal.response")

}
