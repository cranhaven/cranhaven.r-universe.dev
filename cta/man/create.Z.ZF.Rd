\name{create.Z.ZF}
\alias{create.Z.ZF}
\title{
Population Matrix and Sampling Constraint Matrix
}
\description{
Creates the population (aka strata) matrix \eqn{Z} and the sampling constraint
matrix \eqn{Z_{F}} using strata and sampling constraint information found in
input variables \code{strata} and \code{fixed.strata}.
}
\usage{
create.Z.ZF(strata, nrowZ = length(strata), fixed.strata = "all")
}
\arguments{
  \item{strata}{The vector that gives the stratum membership identifier.}
  \item{nrowZ}{Number of rows of the to-be-created population (aka strata)
               matrix \eqn{Z}.}
  \item{fixed.strata}{The object that gives information on which stratum have
                      fixed sample sizes. It can equal one of the keywords,
                      \code{fixed.strata = "all"} or \code{fixed.strata = "none"},
                      or it can be a vector of stratum membership identifiers,
                      e.g. \code{fixed.strata = c(1,3)}. Default:
                      \code{fixed.strata = "all"}.}
}
\value{
\code{create.Z.ZF} returns a list, which includes the following two objects:
  \item{Z}{Population (aka strata) matrix.}
  \item{ZF}{Sampling constraint matrix.}
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.
}
\author{
Joseph B. Lang
}
\seealso{
\code{\link{mph.fit}}
}
\examples{
create.Z.ZF(c(1, 1, 2, 3, 3), fixed.strata = "all")
create.Z.ZF(c(1, 1, 2, 3, 3), fixed.strata = "none")
create.Z.ZF(c(1, 1, 2, 3, 3), fixed.strata = c(1, 2))
}
