\name{plot.spm}
\alias{plot.spm}
\title{Semiparametric regression plotting}
\description{Takes a fitted \code{spm} object 
produced by \code{spm()} and plots the 
component smooth functions that make it up, on the 
scale of the linear predictor.}
\usage{
\method{plot}{spm}(x,...)
}
\arguments{ 
  
\item{x}{a fitted \code{spm} object as produced by \code{spm()}.}

\item{...}{other graphics parameters described in Appendix B of 
the SemiPar Users' Manual \url{http://matt-wand.utsacademics.info/SPmanu.pdf}}

}
\details{Produces plots with each panel corresponding 
to a component of the semiparametric regression model.}

\value{The function generates plots.}

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

\seealso{\code{\link{spm}}
\code{\link{lines.spm}}
\code{\link{predict.spm}}
\code{\link{summary.spm}}}

\examples{
library(SemiPar)
data(fossil)
attach(fossil)
fit <- spm(strontium.ratio~f(age))
plot(fit)

# Now do several customisations

op <- par(bg="white")
par(bg="honeydew")
plot(fit,ylim=range(strontium.ratio),col="green",
     lwd=5,shade.col="mediumpurple1",rug.col="blue")   
points(age,strontium.ratio,col="orange",pch=16)
par(op)

}
\keyword{models} \keyword{smooth} \keyword{regression} 


