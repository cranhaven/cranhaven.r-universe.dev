\name{data.RH.mortality}
\alias{data.RH.mortality}
\alias{data.RH.mortality.dk}
\alias{data.RH.mortality.no}
\title{2-sample mortality data.}
\description{
Function that organises mortality data from Riebler and Held (2010) in \code{\link{apc.data.list}} format.

The data set is taken from the supplementary data of Riebler and Held (2010).
Mortality data for women in Denmark and Norway

The original source was
Jacobsen et al. (2004).

The data set is in "AP"-format.
}
\usage{data.RH.mortality.dk()
data.RH.mortality.no()}

\value{The value is a list in \code{\link{apc.data.list}} format.
	\item{response}{matrix of cases}
	\item{dose}{matrix of cases/rates}
	\item{data.format}{logical equal to "AP".  Data organised with age-groups in rows and period-groups in columns.}
	\item{age1}{numeric equal to 0.}
	\item{per1}{numeric equal to 1960.}
	\item{coh1}{NULL.  Not needed when data.format="AP"}
	\item{unit}{numeric equal to 5.  This is the width of the age and period groups.}
	\item{per.zero}{NULL.  Not needed when data.format="AP"} 
	\item{per.max}{NULL.  Not needed when data.format="AP"} 
	\item{time.adjust}{0.  Thus age=0 in period=1960 corresponds to cohort=1960-0+0=1960, and indeed the
							centers of the age and period groups, that is age=2 and period=1962
							translate into cohort=1962-2+0=1960.}	
	\item{label}{character. "RH mortality Denmark" or "RH mortality Norway".}						
}
\source{Riebler and Held (2010), supplementary material.}
\references{
Jacobsen, R, von Euler, M, Osler, M, Lynge, E and Keiding, N (2004) Women's death in Scandinavia - what makes Denmark different? \emph{European Journal of Epidemiology} 19, 117-121.

Riebler, A and Held, L. (2010) The analysis of heterogeneous time trends in multivariate age-period-cohort models. \emph{Biostatistics} 11, 57--59. \emph{Download}: \doi{https://doi.org/10.1093/biostatistics/kxp037}, see supplementary material.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 17 Sep 2016}
\seealso{General description of \code{\link{apc.data.list}} format.}
\examples{
#########################
##	It is convient to construct a data variable

data	<- data.US.prostate.cancer()

##	To see the content of the data

data
}
