\name{summary.spm}
\alias{summary.spm}
\title{Semiparametric regression summary}
\description{Takes a fitted \code{spm} object produced by 
\code{spm()} and summarises the fit.}
\usage{
\method{summary}{spm}(object,...)
}
\arguments{ 
  
\item{object}{a fitted \code{spm} object as produced by \code{spm()}.}

\item{...}{other arguments.}

}
\details{Produces tables for the linear (parametric)
and non-linear (nonparametric) components. The linear
table provides coefficient estimates, standard errors
and p-values. The non-linear table provides degrees of
freedom values and other information.}

\value{The function generates summary tables.}

\references{
     Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}


     Ganguli, B. and Wand, M.P. (2005)\cr
     \emph{SemiPar 1.0 Users' Manual}.\cr
     \url{http://matt-wand.utsacademics.info/SPmanu.pdf}
}
\author{M.P. Wand 
\email{mwand@uow.edu.au}
(other contributors listed in SemiPar Users' Manual).}

\seealso{
\code{\link{spm}}
\code{\link{plot.spm}}
\code{\link{predict.spm}}
}

\examples{
library(SemiPar)
data(onions)
attach(onions)
log.yield <- log(yield)
fit <- spm(log.yield~location+f(dens))
summary(fit)
}
\keyword{models} \keyword{smooth} \keyword{regression} 


