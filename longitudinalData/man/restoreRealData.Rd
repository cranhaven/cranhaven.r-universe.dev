\name{restoreRealData}
\alias{restoreRealData}
\alias{restoreRealData,LongData}
\alias{restoreRealData,LongData3d}
\alias{restoreRealData,LongData-method}
\alias{restoreRealData,LongData3d-method}

\title{
~ Function: restoreRealData ~
}
\description{
  This function revert the effect of \code{\link{scale}} by restauring
  the initial values of trajectories.
}
\usage{
restoreRealData(object)
}
\arguments{
  \item{object}{\code{[LongData]}: Object containnig trajectories to restore.}
}

\details{
  This function revert the effect of \code{\link{scale}} by restauring
  the initial values of trajectories.
}
\value{
None: this function change internaly the field of an object, it does not return any values.)
}



\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\seealso{\code{\link{scale}}}

\examples{
##################
### Building LongData

time=c(1,2,3,4,8,12,16,20)
id2=1:12
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
ld1 <- longData3d(array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(12*8*2,0,1),dim=c(12,8,2)))
plotTrajMeans3d(ld1)

##################
### Scaling by 'mean' and 'standard deviation'
scale(ld1,scale=c(-1,-1))
plotTrajMeans3d(ld1)

##################
### Back to the first version of the data
restoreRealData(ld1)
plotTrajMeans3d(ld1)
}
