\name{fossil}
\docType{data}
\alias{fossil}
\non_function{}
\title{Fossil data}
\usage{data(fossil)}
\description{
The \code{fossil} data frame has 106 observations on fossil shells.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{age}{age in millions of years}
    \item{strontium.ratio}{ratios of strontium isotopes}
  }
}
\source{
Bralower, T.J., Fullagar, P.D., Paull, C.K., Dwyer, G.S.
and Leckie, R.M. (1997).
Mid-cretaceous strontium-isotope stratigraphy
of deep-sea sections. \emph{Geological Society
of America Bulletin}, \bold{109}, 1421-1442.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(fossil)
attach(fossil)
plot(age,strontium.ratio)
}
\keyword{datasets}
