\name{lines.spm}
\alias{lines.spm}
\title{Add a curves to an existing plot.}
\description{Takes a fitted \code{spm} object produced by \code{spm()} and 
adds a curve. The function is only appropriate in the case 
of a single predictor.}
\usage{
\method{lines}{spm}(x,...)
}
\arguments{ 
\item{x}{a fitted \code{spm} object as produced by \code{spm()}.}
\item{...}{other graphics parameters described in Appendix B of 
the SemiPar Users' Manual \url{http://matt-wand.utsacademics.info/SPmanu.pdf}}
}
\details{Takes a fitted \code{spm} object produced by \code{spm()} and 
adds a curve. The function is only appropriate in the case 
of a single predictor.}

\value{The function adds a curve to a plot.}

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
\code{\link{summary.spm}}
\code{\link{residuals.spm}}
\code{\link{fitted.spm}}
}

\examples{
library(SemiPar)
data(fossil)
attach(fossil)
fit <- spm(strontium.ratio~f(age))
plot(fossil,type="n")
lines(fit)
points(fossil)

# Now do several customisations

op <- par(bg="white")
par(bg="honeydew")
plot(fossil,type="n")
lines(fit,col="green",lwd=5,shade.col="mediumpurple1")   
points(fossil,col="orange",pch=16)
par(op)

}
\keyword{models} \keyword{smooth} \keyword{regression} 


