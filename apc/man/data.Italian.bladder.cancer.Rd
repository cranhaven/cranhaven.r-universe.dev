\name{data.Italian.bladder.cancer}
\alias{data.Italian.bladder.cancer}
\title{Italian bladder cancer data}
\description{
Function that organises Italian bladder data in \code{\link{apc.data.list}} format.

The data set is taken from table IV of Clayton and Schifflers (1987a), which contains age-specific incidence rates (per 100,000 person-years observation) of bladder cancer in Italian males during the period 1955-1979. Numerators are also available. The original source was the WHO mortality database.  

The data set is in "AP"-format.
}
\usage{data.Italian.bladder.cancer()}
\value{The value is a list in \code{\link{apc.data.list}} format.
	\item{rates}{matrix of mortality rates.  This is not needed for the \code{\link{apc.data.list}} format, but included as this is the original data formats}
	\item{response}{matrix of cases}
	\item{dose}{matrix of cases/rates}
	\item{data.format}{logical equal to "AP".  Data organised with age-groups in rows and period-groups in columns.}
	\item{age1}{numeric equal to 25.  This is the label for the first age group covering ages 25-29.}
	\item{per1}{numeric equal to 1955.  This is the label for the first period group covering period 1955-1959.}
	\item{coh1}{NULL.  Not needed when data.format="AP"}
	\item{unit}{numeric equal to 5.  This is the width of the age and period groups.}
	\item{per.zero}{NULL.  Not needed when data.format="AP"} 
	\item{per.max}{NULL.  Not needed when data.format="AP"}
	\item{time.adjust}{0.  Thus age=25 in period=1955 corresponds to cohort=1955-25+0=1930, and indeed the
							centers of the age and period groups, that is age=27 and period=1957
							translate into cohort=1957-27+0=1930.}
	\item{label}{character. "Italian bladder cancer".}													
}
\source{Table IV of Clayton and Schifflers (1987a).}
\references{
Clayton, D. and Schifflers, E. (1987a)
Models for temperoral variation in cancer rates. I: age-period and age-cohort models. 
\emph{Statistics in Medicine} 6, 449-467.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 8 Sep 2015 (24 Oct 2013)}
\seealso{General description of \code{\link{apc.data.list}} format.}
\examples{
#########################
##	It is convient to construct a data variable

data	<- data.Italian.bladder.cancer()

##	To see the content of the data

data
}
