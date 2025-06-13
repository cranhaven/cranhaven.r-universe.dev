\name{data.US.prostate.cancer}
\alias{data.US.prostate.cancer}
\title{Japanese breast cancer data}
\description{
Function that organises US prostate data in \code{\link{apc.data.list}} format.

The data set is taken from table 2 of Holford (1983), which contains age-specific counts of deaths
and midperiod population measured in 1000s,
during the period 1935-1969.
Reported in 5 year age groups and 5 year period groups.

The original source was
Cancer deaths: National Center for Health Statistics, 1937-1973
Population 1935-60: Grove and Hetzel, 1968
Population 1960-69: Bureau of the Census, 1974		


The data set is in "AP"-format.
}
\usage{data.US.prostate.cancer()}
\value{The value is a list in \code{\link{apc.data.list}} format.
	\item{response}{matrix of cases}
	\item{dose}{matrix of cases/rates}
	\item{data.format}{logical equal to "AP".  Data organised with age-groups in rows and period-groups in columns.}
	\item{age1}{numeric equal to 50.  This is the label for the first age group covering ages 25-29.}
	\item{per1}{numeric equal to 1935.  This is the label for the first period group covering period 1955-1959.}
	\item{coh1}{NULL.  Not needed when data.format="AP"}
	\item{unit}{numeric equal to 5.  This is the width of the age and period groups.}
	\item{per.zero}{NULL.  Not needed when data.format="AP"} 
	\item{per.max}{NULL.  Not needed when data.format="AP"} 
	\item{time.adjust}{0.  Thus age=50 in period=1935 corresponds to cohort=1935-50+0=1885, and indeed the
							centers of the age and period groups, that is age=52 and period=1937
							translate into cohort=1937-52+0=1885.}	
	\item{label}{character. "US prostate cancer".}						
}
\source{Table 2 of Holford (1983)}
\references{
Holford, T.R. (1983) The estimation of age, period and cohort effects for vital rates. \emph{Biometrics} 39, 311-324.                                             
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 8 Sep 2015 (28 Apr 2015)}
\seealso{General description of \code{\link{apc.data.list}} format.}
\examples{
#########################
##	It is convient to construct a data variable

data	<- data.US.prostate.cancer()

##	To see the content of the data

data
}
