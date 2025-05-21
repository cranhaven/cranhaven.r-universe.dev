\name{janka}
\alias{janka}
\non_function{}
\title{Janka hardness data}
\usage{data(janka)}
\description{
The \code{janka} data frame has 36 observations on Australian timber
samples.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{dens}{a measure of density of the timber.}
    \item{hardness}{the Janka hardness (structural property) of the timber.}
  }
}
\source{
Williams, E.J. (1959) \emph{Regression Analysis}, New York:
John Wiley & Sons.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(janka)
attach(janka)
plot(dens,hardness)
}
\keyword{datasets}
