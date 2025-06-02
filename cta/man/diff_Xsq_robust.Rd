\name{diff_Xsq_robust}
\alias{diff_Xsq_robust}
\title{
Difference in \eqn{X}-Squared Statistic Based CIs (Robust)
}
\description{
Constructs confidence intervals (CIs), based on the difference in \eqn{X^2} statistic, for estimands in contingency tables subject to equality constraints.
}
\usage{
diff_Xsq_robust(y, strata, fixed.strata, h0.fct, h0.fct.deriv,
                S0.fct, S0.fct.deriv, max.mph.iter, step,
                change.step.after, y.eps, iter.orig, norm.diff.conv,
                norm.score.conv, max.score.diff.iter, S.space.H0,
                tol.psi, tol, max.iter, cut.off, delta, adj.epsilon,
                iter.robust.max, iter.robust.eff)
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
                      the estimand function \eqn{S_{0}(\cdot)} with respect
                    to \eqn{m}. If \code{S0.fct.deriv} is not
                    specified or \code{S0.fct.deriv = NULL}, numerical derivatives
                    will be used.}
  \item{max.mph.iter, step, change.step.after, y.eps, iter.orig, norm.diff.conv,
  norm.score.conv, max.score.diff.iter}{The parameters used in \code{mph.fit}.}
  \item{S.space.H0}{Restricted estimand space of \eqn{S(\cdot)} under \eqn{H_{0}}, i.e.
                  subject to the imposed equality constraints along with sampling
                  constraints.}
  \item{tol.psi, tol, max.iter}{The parameters used in the three stopping criteria in solving for the roots to the test-inversion equation.}
\item{cut.off}{\code{qchisq(cc, 1)}. i.e. The chi-square cutoff, with \eqn{1}
               df, based on the significance level \code{1-cc}.}
\item{delta}{The constant \eqn{\delta} that is in expressions of the moving critical
             values within each sliding quadratic step.}
  \item{adj.epsilon, iter.robust.max, iter.robust.eff}{The parameters used in the robustifying procedure.}
}
\value{
\code{diff_Xsq_robust} returns a list, which includes two objects. The first object is a \eqn{1}-by-\eqn{2} matrix which displays two endpoints of the confidence interval based on the difference in \eqn{X^2} statistic. For the second object, it includes the warning message that occurs during construction of the confidence
interval if the robustifying procedure is evoked: \code{"diff.Xsq.CI: Adjustment used. Not on original data.\n"}. If the robustifying procedure is not evoked, the second object is \code{NULL}.
}
\references{
Zhu, Q. (2020) "On improved confidence intervals for parameters of discrete distributions." PhD dissertation, University of Iowa.
}
\author{
Qiansheng Zhu
}
\seealso{
\code{\link{diff_Xsq_nr}}, \code{\link{f.psi}}, \code{\link{ci.table}}
}
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
