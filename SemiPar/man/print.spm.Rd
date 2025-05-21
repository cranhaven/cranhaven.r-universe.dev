\name{print.spm}
\alias{print.spm}
\title{Prints semiparametric regression fit object.}
\description{Prints a brief description of a 
semiparametric regression fit object to the screen.}
\usage{
\method{print}{spm}(x,...)
}
\arguments{ 
  
\item{x}{a fitted \code{spm} object as produced by \code{spm()}.}

\item{...}{other possible arguments.}

}
\details{Prints a brief description of a 
semiparametric regression fit object to the screen.}

\value{The function prints to the screen.}

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
\code{\link{plot.spm}} 
\code{\link{lines.spm}}
\code{\link{predict.spm}}
\code{\link{summary.spm}}
\code{\link{residuals.spm}}
\code{\link{fitted.spm}}}

\examples{
library(SemiPar)
data(fossil)
attach(fossil)
fit <- spm(strontium.ratio~f(age))
print(fit)
}
\keyword{models} \keyword{smooth} \keyword{regression} 


