\name{age.income}
\docType{data}
\alias{age.income}
\non_function{}
\title{Age/income data}
\usage{data(age.income)}
\description{
The \code{age.income} data frame has 205 pairs observations on 
Canadian workers from a 1971 Canadian Census Public Use Tape
(Ullah, 1985).
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{age}{age in years.}
    \item{log.income}{logarithm of income.}
  }
}
\source{
Ullah, A. (1985). Specification analysis of econometric
models. \emph{Journal of Quantitative Economics},
\bold{2}, 187-209.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(age.income)
attach(age.income)
plot(age,log.income)
}
\keyword{datasets}
