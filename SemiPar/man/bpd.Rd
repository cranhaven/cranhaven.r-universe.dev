\name{bpd}
\alias{bpd}
\non_function{}
\title{Bronchopulmonary dysplasia data}
\usage{data(bpd)}
\description{
The \code{bpd} data frame has data on 223 human babies.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{birthweight}{birthweight of baby (grammes).}
    \item{BPD}{an indicator of presence of bronchopulmonary dysplasia (BPD):
               0=absent, 1=present.}
  }
}
\source{
Pagano, M. and Gauvreau, K. (1993).
\emph{Principles of Biostatistics}. Duxbury Press.

}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(bpd)
attach(bpd)
plot(birthweight,BPD)
boxplot(split(birthweight,BPD),col="green")
}
\keyword{datasets}
