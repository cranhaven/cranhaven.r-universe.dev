\name{scale}
\alias{scale}
\alias{scale,LongData}
\alias{scale,LongData3d}
\alias{scale,LongData-method}
\alias{scale,LongData3d-method}

\title{
  ~ Function: scale for LongData ~
}
\description{
\code{scale} the trajectories of the different variable of a
  \code{\linkS4class{LongData}} object.
}
\usage{
scale(x, center = TRUE, scale = TRUE)
}

\arguments{
  \item{x}{\code{[LongData]}: Object containnig trajectories to be scale.}
  \item{center}{\code{[logical]} or \code{[vector(numeric)]}: Value that
    will be substract from each mesurement of a variable. If \code{center=TRUE}, the mean of
    each variable-trajectory is used. Otherwise, \code{center} should have a value
    for each variables.}
  \item{scale}{\code{[logical]} or \code{[vector(numeric)]}: Value that
    will divided, after the substration, each mesurement of a variable.
    If \code{scale=TRUE}, the standard deviation of
    each variable-trajectory is used. Otherwise, \code{scale} should have a value
    for each variables.}
}

\details{
  When variable with different unit are used jointly, it might be necessary to
  change their scale them in order to change their individual influance.
  This is what \code{scale} do.

  More precisely, all the value x[i,j,k] of the variable k will be scale
  according to the classic formula \code{(x[i,j,k]- m_k)/s_k} where
  m_k and s_k are respectively the k-ieme value of the argument
  \code{center} and \code{scale}.

  Note that \code{center=TRUE} is a special value that set \code{m_k=mean(x[,,k],na.rm=TRUE)}.
  Similarly, \code{scale=TRUE} is a special value that set \code{s_k=sd(x[,,k],na.rm=TRUE)}.
}

\value{
  \code{scale} directly
  modify the internal value of the \code{LongData}. No value is return.
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSM, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}


\seealso{\code{\link{restoreRealData}}}

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
plotTrajMeans3d(ld1)
scale(ld1)
plotTrajMeans3d(ld1)

### Scaling by some parameters
scale(ld1,center=c(10,100),scale=c(3,-1))
plotTrajMeans3d(ld1)

##################
### To restore the data
restoreRealData(ld1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{method}
