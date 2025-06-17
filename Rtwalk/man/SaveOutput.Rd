\name{SaveOutput}

\alias{SaveOutput}

\title{Save the twalk MCMC output to a file.}

\description{
Save the twalk MCMC output to a file.
}

\usage{
SaveOutput( info, file, pars=1:(info$dim), from=1, to=info$Tr,
	row.names=FALSE, col.names=paste("X", pars), ...)
}

\arguments{

    \item{info}{
        as returned from Runtwalk.
        }
    \item{file}{
        name of file to write results to (of not "").
	}
    \item{from}{
        iteration number to start saving (from=0 begings at initialization point).
        } 
    \item{to}{
        last iteration to save.
        }
    \item{pars}{
        parameters to save (defaults to all).
        }
    \item{row.names, col.names, ...}{
        parameters passed to write.table.
	}
 }

\value{
write.table object.
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


### Save the twalk MCMC output as columns in a table
SaveOutput( info, file="Tsttwalk.dat")

}


\keyword{Output}
\keyword{MCMC}

