\name{Ana}

\alias{Ana}

\title{Perform some basic autocorrelation analysis.}

\description{
Perform some basic autocorrelation analysis of the twalk MCMC output.
}

\usage{
Ana(info, from=1, to=info$Tr, par=0, file="")
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
    \item{par}{
        parameter to analyze.
        }
    \item{file}{
        name of file to write results to (if not "").
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

### Perform some basic autocorrelation analysis
Ana( info, from=500)

}


\keyword{Autocorrelations}
\keyword{IAT}
\keyword{MCMC}

