\name{fitted.spm}
\alias{fitted.spm}
\title{Fitted values for semiparametric regression.}
\description{Extracts fitted values from a semiparametric 
regression fit object.}
\usage{
\method{fitted}{spm}(object,...)
}
\arguments{ 
  
\item{object}{a fitted \code{spm} object as produced by \code{spm()}.}

\item{...}{other possible arguments.}

}
\details{Extracts fitted from a semiparametric regression fit object.
The fitted are defined to be the set of values obtained
when the predictor variable data are substituted into the
fitted regression model.}

\value{The vector of fitted.}

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
\code{\link{lines.spm}}
\code{\link{predict.spm}}
\code{\link{summary.spm}}
\code{\link{residuals.spm}}
}

\examples{
library(SemiPar)
data(fossil)
attach(fossil)
fit <- spm(strontium.ratio~f(age))
plot(fit)
points(age,fitted(fit),col="red")
}
\keyword{models} \keyword{smooth} \keyword{regression} 


