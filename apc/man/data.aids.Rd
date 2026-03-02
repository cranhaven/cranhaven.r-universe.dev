\name{data.aids}
\alias{data.aids}
\title{UK aids data}
\description{
Function that organises UK aids data in \code{\link{apc.data.list}} format.

The data set is taken from table 1 of De Angelis and Gilks (1994). The data
are also analysed by Davison and Hinkley (1998, Example 7.4).
The data are reporting delays for AIDS counting	the number of cases by the
date of diagnosis and length of reporting delay, measured by quarter. 

The data set is in "trapezoid"-format.
The original data set is unbalanced in various ways:
first column covers a reporting delay of less than one month (or should it be less than one quarter?);
last column covers a reporting delay of at least 14 quarters;
last diagonal include incomplete counts.
The default data set excludes the incomplete counts in the last diagonal, but
includes the unbalanced first and last columns.
}
\usage{data.aids(all.age.groups = FALSE)}
\arguments{
  	\item{all.age.groups}{logical.  If FALSE (default), the last calendar year with incomplete counts is ignored.}
}
\value{The value is a list in \code{\link{apc.data.list}} format.
	\item{response}{matrix of cases}
	\item{data.format}{logical equal to "trapezoid".}
	\item{age1}{numeric equal to 0.  This is the label for the reporting delay.}
	\item{per1}{NULL.  Not needed when data.format="trapezoid"}
	\item{coh1}{numeric equal to 1983.5.  This is the label for the diagnosis quarter (1983, third quarter).}
	\item{unit}{numeric equal to 1/4.  This is the width of the age and period groups.}
	\item{per.zero}{numeric equal to 0.} 
	\item{per.max}{numeric equal to 38.} 
	\item{time.adjust}{numric equal to 0.}
	\item{label}{character.  Default data has "UK AIDS - clean".}						
}
\source{Table 1 of De Angelis and Gilks (1994).  Also analysed by Davison and Hinkley (1998, Example 7.4).}
\references{
De Angelis, D. and Gilks, W.R. (1994) Estimating acquired immune deficiency syndrome incidence accounting for reporting delay. \emph{Journal of the Royal Statistical Sociey} A 157, 31-40.

Davison, A.C. and Hinkley, D.V. (1998) \emph{Bootstrap methods and their application}. Cambridge: Cambridge University Press.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 7 Feb 2016}
\seealso{General description of \code{\link{apc.data.list}} format.}
\examples{
#########################
##	It is convient to construct a data variable
data	<- data.Belgian.lung.cancer()
##	To see the content of the data
data

#########################
#	Forecast AIDS incidences by diagonsis year (cohort).
#	uses as poisson response model with an AC structure
#	although there is evidence of overdispersion and the
#	period effect appears significant.
#	The omission of the period effect follows
#	Davison and Hinkley and a parsimoneous model may be
#	advantageous when forecasting.
#
apc.fit.table(data.aids(),"poisson.response")
fit <- apc.fit.model(data.aids(),"poisson.response","AC")
forecast <- apc.forecast.ac(fit)
data.sums.coh <- apc.data.sums(data.aids())$sums.coh
forecast.total <- forecast$response.forecast.coh
forecast.total[,1]	<- forecast.total[,1]+data.sums.coh[25:38]
x	<- seq(1983.5,1992.75,by=1/4)
y	<- data.sums.coh
xlab<- "diagnosis year (cohort)"
ylab<- "diagnoses"
main<- "Davison and Hinkley, Fig 7.6, parametric version"
plot(x,y,xlim=c(1988,1993),ylim=c(200,600),xlab=xlab,ylab=ylab,main=main)
apc.polygon(forecast.total,x.origin=1989.25,unit=1/4)
}
