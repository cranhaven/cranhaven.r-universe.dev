\name{TS}

\alias{TS}

\title{Plot a 'time series' of the twalk MCMC output.}

\description{
Plot a 'time series' of the twalk MCMC output.
}

\usage{
TS(info, pars=1:(info$dim), from=1, to=info$Tr, prime=FALSE)
}

\arguments{

    \item{info}{
        as returned from Runtwalk.
        }
    \item{pars}{
        parameter list to plot.
        }
    \item{from}{
        iteration number to start ploting (from=0 begings at initialization point).
        } 
    \item{to}{
        last iteration to plot.
        }
    \item{prime}{
        plot xp (x') instead.
        }
 }

\value{
plot object.
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

### One can plot a time series of the parameters
TS(info)
}

\keyword{Time series}
\keyword{twalk}
\keyword{MCMC}
