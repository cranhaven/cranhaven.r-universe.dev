\name{bic.spfac}
\alias{bic.spfac}
\title{Modified BIC criteria for selecting penalty parameters}
\description{
  Evalute the BIC values on a set of grids of penalty parameters.
}
\usage{
  bic.spfac(X, c1.max= 10, nlamb1=10, C10=4, c2.max=10, nlamb2=10, C20=4)
  }
\arguments{
  \item{X}{a \code{n}-by-\code{p} matrix, the observed data}
  \item{c1.max}{a positve scalar, the maximum of the grids of c1.}
  \item{nlamb1}{a positive integer, the length of grids of penalty parameter lambda1.}
  \item{C10}{a positve scalar, the penalty factor C1 of modified BIC.}
  \item{c2.max}{a positve scalar, the maximum of the grids of c2.}
  \item{nlamb2}{a positive integer, the length of grids of penalty parameter lambda2.}
  \item{C20}{a positve scalar, the penalty factor C2 of modified BIC.}
}

\value{
  return a list with class named \code{pena_info} and \code{BIC}, including following components:

  \item{lambda1.min}{a positive number, the penalty value for lambda1 corresponding to the minimum BIC on grids.}
  \item{lambda2.min}{a positive number, the penalty value for lambda2 corresponding to the minimum BIC on grids.}
  \item{bic1}{a numeric matrix with three columns named c1, lambda1 and bic1, where each row is corresponding to each grid.}
  \item{bic2}{a numeric matrix with three columns named c2, lambda2 and bic2, where each row is corresponding to each grid.}
}
\references{Wei Liu, Huazhen Lin, Jin Liu (2020). Estimation and inference on high-dimensional sparse factor models.
}
\author{
  Liu Wei
}
\note{
  nothing
}
\seealso{
  \code{\link{gsspFactorm}}.
}
\examples{
  datlist1 <- gendata_Fac(n= 100, p = 500)
  X <- datlist1$X
  spfac <- gsspFactorm(X, q=NULL) # use default values for lambda's.
  assessBsFun(spfac$sphB, datlist1$B0)

  biclist <- bic.spfac(datlist1$X, c2.max=20,nlamb1 = 10) # # select lambda's values using BIC.
}
