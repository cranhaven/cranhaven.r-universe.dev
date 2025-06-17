\name{PlotHist}

\alias{PlotHist}

\title{Plot a histogram of a }

\description{
Plot a histogram of the twalk MCMC output.
}

\usage{
PlotHist( info, par=1, from=0, xlab=paste("Parameter", par), main="", ...)
}

\arguments{

    \item{info}{
        as returned from Runtwalk.
        }
    \item{par}{
        parameter number to plot.
        }
    \item{from}{
        iteration number to start ploting (from=0 begings at initialization point).
        } 
    \item{xlab}{
        parameter passed to hist.
        }
    \item{main}{
        parameter passed to hist.
        }
    \item{...}{
        parameters passed to hist.
        }
 }

\value{
hist object.
}


\references{
Christen JA and Fox C (2010). A general purpose sampling algorithm for continuous distributions (the t-walk)., Bayesian Analysis, 5 (2), 263-282. URL: http://ba.stat.cmu.edu/journal/2010/vol05/issue02/christen.pdf
}


\author{
  J Andres Christen (CIMAT, Guanajuato, MEXICO).
}

\seealso{\code{\link{Runtwalk}} For running the twalk.}

\examples{  

#### We first load the twalk package:
library(Rtwalk)



#### A ver simple inline example, 4 independent normals N(0,1):
######        dimension,  num of it, -log of objective function besides a const, support,
info <- Runtwalk( dim=4,  Tr=1000,  Obj=function(x) { sum(x^2)/2 }, Supp=function(x) { TRUE },
	x0=runif(4, min=20, max=21), xp0=runif(4, min=20, max=21)) 
####  and two (intentionally bad) initial points

### One can plot some histograms:
PlotHist(info, par=3)
}

\keyword{output}
\keyword{twalk}
\keyword{MCMC}
