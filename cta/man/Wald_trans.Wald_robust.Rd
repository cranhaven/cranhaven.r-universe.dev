\name{Wald_trans.Wald_robust}
\alias{Wald_trans.Wald_robust}
\title{
Wald-Type CIs (Robust)
}
\description{
Constructs non-transformed and transformed (if the
transformation \eqn{g} is specified) Wald confidence intervals (CIs) for estimands in contingency tables subject to equality constraints.
}
\usage{
Wald_trans.Wald_robust(y, strata, fixed.strata, h0.fct, h0.fct.deriv,
                       S0.fct, S0.fct.deriv, max.mph.iter, step,
                       change.step.after, y.eps, iter.orig, norm.diff.conv,
                       norm.score.conv, max.score.diff.iter, cut.off,
                       S.space.H0, trans.g, trans.g.deriv, trans.g.inv,
                       adj.epsilon, iter.robust.max, iter.robust.eff)
}
\arguments{
  \item{y}{Observed table counts in the contingency table(s), in vector form.}
  \item{strata}{Vector of the same length as \code{y} that gives the stratum
                membership identifier.}
  \item{fixed.strata}{The object that gives information on which stratum (strata) has
                    (have) fixed sample sizes.}
  \item{h0.fct}{The constraint function \eqn{h_{0}(\cdot)} with respect to \eqn{m}, where
              \eqn{m = E(Y)}, the vector of expected table counts.}
  \item{h0.fct.deriv}{The \R function object that computes analytic derivative of the
                    transpose of the constraint function \eqn{h_{0}(\cdot)} with respect to
                    \eqn{m}. If \code{h0.fct.deriv} is not specified or
                    \code{h0.fct.deriv = NULL}, numerical derivatives will be used.}
  \item{S0.fct}{The estimand function \eqn{S_{0}(\cdot)} with respect to \eqn{m}.}
  \item{S0.fct.deriv}{The \R function object that computes analytic derivative of
                    the estimand function \eqn{S_{0}(\cdot)} with
                    respect to \eqn{m}. If \code{S0.fct.deriv} is not
                    specified or \code{S0.fct.deriv = NULL}, numerical derivatives
                    will be used.}
  \item{max.mph.iter, step, change.step.after, y.eps, iter.orig, norm.diff.conv,
  norm.score.conv, max.score.diff.iter}{The parameters used in \code{mph.fit}.}
  \item{cut.off}{\code{qchisq(cc, 1)}. i.e. The chi-square cutoff, with \eqn{1}
                 df, based on the significance level \code{1-cc}.}
  \item{S.space.H0}{Restricted estimand space of \eqn{S(\cdot)} under \eqn{H_{0}}, i.e.
                  subject to the imposed equality constraints along with sampling
                  constraints.}
  \item{trans.g}{The transformation \eqn{g} used in the transformed Wald confidence interval.}
  \item{trans.g.deriv}{The derivative function of the transformation \eqn{g}, i.e. \eqn{d g(w) / d w}. If it is specified, it should be an \R function, even if the derivative function is a constant function.}
  \item{trans.g.inv}{\eqn{g^{-1}} function used in back-transformation step in construction of the transformed Wald confidence interval.}
  \item{adj.epsilon, iter.robust.max, iter.robust.eff}{The parameters used in the robustifying procedure.}
}
\value{
\code{Wald_trans.Wald_robust} returns a list, which includes two objects. The first object is
\itemize{
\item either a \eqn{1}-by-\eqn{2} matrix which displays two endpoints of the non-transformed Wald confidence interval, if the
transformation \eqn{g} is not specified;
\item or a \eqn{2}-by-\eqn{2} matrix, whose first row displays two endpoints of the non-transformed Wald confidence interval, and whose second row displays two endpoints of the transformed Wald confidence interval, if the transformation \eqn{g} is specified.
}
For the second object, it includes the warning message that occurs during construction of the confidence
interval(s) if the robustifying procedure is evoked: \code{"Wald.CI: Adjustment used. Not on original data.\n"}, or \code{"Wald.CI and trans.Wald.CI: Adjustment used. Not on original data.\n"}. If the robustifying procedure is not evoked, the second object is \code{NULL}.
}
\references{
Zhu, Q. (2020) "On improved confidence intervals for parameters of discrete distributions." PhD dissertation, University of Iowa.
}
\author{
Qiansheng Zhu
}
\seealso{
\code{\link{Wald_trans.Wald_nr}}, \code{\link{f.psi}}, \code{\link{ci.table}}
}
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
