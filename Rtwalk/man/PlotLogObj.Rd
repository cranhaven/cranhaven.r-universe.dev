\name{PlotLogObj}

\alias{PlotLogObj}

\title{Plot a trace of the Log of Objective function values}

\description{
Plot a trace of the Log of Objective function values, burn-in for convergence evaluation purposes.
}

\usage{
PlotLogObj(info, from=0, to=length(info$Us))
}

\arguments{

    \item{info}{
        as returned from Runtwalk.
        }
    \item{from}{
        iteration number to start ploting (from=0 begings at initialization point).
        } 
    \item{to}{
        last iteration to plot.
        }
 }

\value{
NULL
}


\references{
Christen JA and Fox C (2010). A general purpose sampling algorithm for continuous distributions (the t-walk)., Bayesian Analysis, 5 (2), 263-282. URL: http://ba.stat.cmu.edu/journal/2010/vol05/issue02/christen.pdf
}


\author{
  J Andres Christen (CIMAT, Guanajuato, MEXICO).
}

\seealso{\code{\link{Runtwalk}} for running the twalk.}

\examples{  

#### We first load the twalk package:
library(Rtwalk)



#### A ver simple inline example, 4 independent normals N(0,1):
######        dimension,  num of it, -log of objective function besides a const, support,
info <- Runtwalk( dim=4,  Tr=1000,  Obj=function(x) { sum(x^2)/2 }, Supp=function(x) { TRUE },
	x0=runif(4, min=20, max=21), xp0=runif(4, min=20, max=21)) 
####  and two (intentionally bad) initial points

### or plot the log of the objective
PlotLogObj(info)
}



\keyword{Plot}
\keyword{Log Objective}
\keyword{MCMC}
