\name{check.homog}
\alias{check.homog}
\title{
\eqn{Z} Homogeneity Check
}
\description{
Checks whether the constraint function \eqn{h(\cdot)} satisfies
a necessary condition for \eqn{Z} homogeneity.
}
\usage{
check.homog(h.fct, Z, tol = NULL)
}
\arguments{
  \item{h.fct}{An \R function object, indicating the constraint function
               \eqn{h(\cdot)} for \eqn{Z} homogeneity check.}
  \item{Z}{Population (aka strata) matrix \eqn{Z}.}
  \item{tol}{The pre-set tolerance with which \code{norm(diff)} is to be compared
             with.}
}
\details{
The main idea:

\eqn{h(\cdot)} is \eqn{Z} homogeneous if \eqn{h(Diag(Z\gamma)x) = G(\gamma)h(x)}, where
\eqn{G} is a diagonal matrix with \eqn{\gamma} elements raised to some power.

As a check,  if \eqn{h(\cdot)} is homogeneous then
\deqn{h(Diag(Z\gamma) x_{1}) / h(Diag(Z\gamma) x_{2}) = h(x_{1}) / h(x_{2});}
That is, \deqn{\texttt{diff} = h(Diag(Z\gamma) x_{1}) h(x_{2}) - h(Diag(Z\gamma) x_{2}) h(x_{1}) = 0.} Here, the division and multiplication are taken element-wise.

This program randomly generates \code{gamma}, \code{x1}, and \code{x2}, and
computes \code{norm(diff)}. It returns a warning if \code{norm(diff)} is
too far from \eqn{0}.
}
\value{
\code{check.homog} returns a character string \code{chk} that states whether
\eqn{h(\cdot)} is \eqn{Z} homogeneous. If \code{chk = ""}, it means that based on the necessary condition, we cannot state that \eqn{h(\cdot)} is not \eqn{Z} homogeneous.
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.
}
\author{
Joseph B. Lang
}
\seealso{
\code{\link{check.zero.order.homog}}, \code{\link{mph.fit}}, \code{\link{check.HLP}}
}
\examples{
# EXAMPLE 1
h.fct <- function(m) {m[1] - m[2]}
Z <- matrix(c(1, 1), nrow = 2)
check.homog(h.fct, Z)

# EXAMPLE 2
h.fct.2 <- function(m) {m[1]^2 - m[2]}
Z <- matrix(c(1, 1), nrow = 2)
check.homog(h.fct.2, Z)
}
