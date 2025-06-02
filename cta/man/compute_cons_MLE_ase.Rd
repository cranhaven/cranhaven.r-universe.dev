\name{compute_cons_MLE_ase}
\alias{compute_cons_MLE_ase}
\title{
Constrained MLE and ASE
}
\description{
Computes the constrained MLE of \eqn{S_{0}(m)} subject to equality constraints \eqn{h_{0}(m) = 0} under the specified \code{strata} and \code{fixed.strata} configuration, and its associated asymptotic standard error. Here \eqn{m} is the vector of expected table counts, i.e. \eqn{m = E(Y)}.
}
\usage{
compute_cons_MLE_ase(y, strata, fixed.strata, h0.fct, h0.fct.deriv, S0.fct,
                     S0.fct.deriv, max.mph.iter, step, change.step.after,
                     y.eps, iter.orig, norm.diff.conv, norm.score.conv,
                     max.score.diff.iter)
}
\arguments{
\item{y}{Observed table counts in the contingency table(s), in vector form.}
\item{strata}{Vector of the same length as \code{y} that gives the stratum membership
              identifier.}
\item{fixed.strata}{The object that gives information on which stratum (strata) has
                    (have) fixed sample sizes.}
\item{h0.fct}{The constraint function \eqn{h_{0}(\cdot)} with respect to \eqn{m}, where
              \eqn{m = E(Y)}, the vector of expected table counts.}
\item{h0.fct.deriv}{The \R function object that computes analytic derivative of the
                    transpose of the constraint function \eqn{h_{0}(\cdot)} with respect to
                    \eqn{m}. If \code{h0.fct.deriv} is not specified or
                    \code{h0.fct.deriv = NULL}, numerical derivatives will be used.}
\item{S0.fct}{The estimand function \eqn{S_{0}(\cdot)} with respect to \eqn{m}.}
\item{S0.fct.deriv}{The \R function object that computes analytic derivative of the estimand function \eqn{S_{0}(\cdot)} with respect to
                    \eqn{m}. If \code{S0.fct.deriv} is not
                    specified or \code{S0.fct.deriv = NULL}, numerical derivatives
                    will be used.}
\item{max.mph.iter, step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv, max.score.diff.iter}{The parameters used in \code{mph.fit}.}
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.

Zhu, Q. (2020) "On improved confidence intervals for parameters of discrete distributions." PhD dissertation, University of Iowa.
}
\value{
\code{compute_cons_MLE_ase} returns a vector of length two. The first element \code{S0.fct.m_H0} is the constrained MLE of \eqn{S_{0}(m)} subject to equality
constraints \eqn{h_{0}(m) = 0}, and the second element \code{ase.S0.fct.m_H0} is the
associated asymptotic standard error.
}
\author{
Qiansheng Zhu
}
\seealso{
\code{\link{ci.table}}
}
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
