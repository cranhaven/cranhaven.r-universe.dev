\name{sitka}
\alias{sitka}
\non_function{}
\title{Sitka spruce data}
\usage{data(sitka)}
\description{
The \code{sitka} data frame contains measurements
of log-size for 79 Sitka spruce trees grown in normal
or ozone-enriched environments. Within each year, the
data are organised in four blocks, corresponding to
four controlled environment chambers. The first
two chambers, containing 27 trees each, have
an ozone-enriched atmosphere, the remaining two,
containing 12 and 13 trees respectively, have a normal
(control) atmosphere.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{id.num}{identification number of tree.}
    \item{order}{time order ranking within each tree.}
    \item{days}{time in days since 1st January, 1988.}
    \item{log.size}{tree size measured on a logarithmic scale.}
    \item{ozone}{indicator ozone treatment: 0=control,1=ozone.}
  }
}
\source{
Diggle, P.J., Heagerty, P., Liang, K.-Y. and Zeger, S.L. (2002).
\emph{Analysis of Longitudinal Data, Second Edition}, Oxord: Oxford
University Press.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(sitka)
attach(sitka)
library(lattice)
ozone.char <- rep("control",nrow(sitka))
ozone.char[ozone==1] <- "ozone"
xyplot(log.size~days|ozone.char,data=sitka,groups=id.num,type="b")
}
\keyword{datasets}
