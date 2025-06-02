\name{check.zero.order.homog}
\alias{check.zero.order.homog}
\title{
Zero-Order \eqn{Z} Homogeneity Check
}
\description{
Checks whether the estimand function \eqn{S(\cdot)} is zero-order \eqn{Z} homogeneous.
}
\usage{
check.zero.order.homog(S.fct, Z, tol = 1e-9)
}
\arguments{
  \item{S.fct}{An \R function object, indicating the estimand function
               \eqn{S(\cdot)} for zero-order \eqn{Z} homogeneity check.}
  \item{Z}{Population (aka strata) matrix \eqn{Z}.}
  \item{tol}{The pre-set tolerance with which \code{norm(diff.LRHS)} is
             to be compared with.}
}
\details{
The main idea:

\eqn{S(\cdot)} is zero-order \eqn{Z} homogeneous if \eqn{S(Diag(Z\gamma) x) = S(x)}, for all \eqn{\gamma > 0}, and for all \eqn{x} within its domain. This program randomly generates \code{gam} (\eqn{\gamma}) and \code{x} (\eqn{x}), and computes \deqn{\texttt{diff.LRHS} = S(Diag(Z\gamma) x) - S(x).} It returns a warning if \code{norm(diff.LRHS)} is too far from \eqn{0}.
}
\value{
\code{check.zero.order.homog} returns a character string \code{check.result} that states whether \eqn{S(\cdot)} is zero-order \eqn{Z} homogeneous. If \code{check.result = ""}, it means that we cannot state that \eqn{S(\cdot)} is not zero-order \eqn{Z} homogeneous based on the result of the check.
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.
}
\author{
Qiansheng Zhu
}
\seealso{
\code{\link{check.homog}}, \code{\link{check.HLP}}
}
\examples{
# EXAMPLE 1
S.fct <- function(m) {(m[1] - m[2]) / (m[1] + m[2])}
Z <- matrix(c(1, 1, 1, 1), nrow = 4)
check.zero.order.homog(S.fct, Z)

# EXAMPLE 2
S.fct.2 <- function(m) {m[1] - m[2]}
Z <- matrix(c(1, 1, 1, 1), nrow = 4)
check.zero.order.homog(S.fct.2, Z)
}
