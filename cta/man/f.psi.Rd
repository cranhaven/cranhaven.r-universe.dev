\name{f.psi}
\alias{f.psi}
\title{
Model Comparison Statistics
}
\description{
Computes one of the model comparison statistics.

The model comparison statistics include:
\itemize{
\item \code{"diff.Gsq"}: The difference in \eqn{G^2} statistic, \deqn{G^{2}(\psi) - G^2 = G^{2}(y; H_{0}(\psi)) - G^{2}(y; H_{0});}
\item \code{"diff.Xsq"}: The difference in \eqn{X^2} statistic, \deqn{X^{2}(\psi) - X^2 = X^{2}(y; H_{0}(\psi)) - X^{2}(y; H_{0});}
\item \code{"diff.PD"}: The difference in power-divergence statistic, with index parameter \eqn{\lambda}, \deqn{PD_{\lambda}(\psi) - PD_{\lambda} = PD_{\lambda}(y; H_{0}(\psi)) - PD_{\lambda}(y; H_{0});}
\item \code{"nested.Gsq"}: The nested \eqn{G^2} statistic, \deqn{G^{2}(y; H_{0}(\psi) | H_{0});}
\item \code{"nested.Xsq"}: The nested \eqn{X^2} statistic, \deqn{X^{2}(y; H_{0}(\psi) | H_{0});}
\item \code{"nested.PD"}: The nested power-divergence statistic, with index parameter
\eqn{\lambda}, \deqn{PD_{\lambda}(y; H_{0}(\psi) | H_{0}).}
}
}
\usage{
f.psi(y, strata, fixed.strata, h0.fct, h0.fct.deriv = NULL,
      S0.fct, S0.fct.deriv = NULL, method_specific, psi,
      max.mph.iter, step, change.step.after, y.eps, iter.orig,
      norm.diff.conv, norm.score.conv, max.score.diff.iter,
      pdlambda = NULL, Gsq_H0, Xsq_H0, PD_H0, cons.MLE.m_H0)
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
  \item{S0.fct.deriv}{The \R function object that computes analytic derivative of                       the estimand function \eqn{S_{0}(\cdot)} with respect to
                    \eqn{m}. If \code{S0.fct.deriv} is not
                    specified or \code{S0.fct.deriv = NULL}, numerical derivatives
                    will be used.}
  \item{method_specific}{A character string that indicates which model comparison
                         statistic to compute. It can be one of \code{"diff.Xsq"},
                         \code{"nested.Xsq"}, \code{"diff.Gsq"}, \code{"nested.Gsq"},
                         \code{"diff.PD"}, or \code{"nested.PD"}.}
  \item{psi}{The real number \eqn{\psi} in the model comparison statistic.}
  \item{max.mph.iter, step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv, max.score.diff.iter}{The parameters used in \code{mph.fit}.}
  \item{pdlambda}{The index parameter \eqn{\lambda} in the power-divergence statistic.}
  \item{Gsq_H0}{The \eqn{G^2} statistic for testing \eqn{H_{0}} vs. not \eqn{H_{0}}, i.e.
                \eqn{G^{2}(y; H_{0})}.}
  \item{Xsq_H0}{The \eqn{X^2} statistic for testing \eqn{H_{0}} vs. not \eqn{H_{0}}, i.e.
                \eqn{X^{2}(y; H_{0})}.}
  \item{PD_H0}{The power-divergence statistic for testing \eqn{H_{0}} vs. not \eqn{H_{0}},
               i.e. \eqn{PD_{\lambda}(y; H_{0})}.}
  \item{cons.MLE.m_H0}{Constrained MLE of \eqn{m = E(Y)} subject to \eqn{H_{0}}.}
}
\note{
Among the four inputs: \code{Gsq_H0}, \code{Xsq_H0}, \code{PD_H0}, and
\code{cons.MLE.m_H0}, only one of them needs to be specified.
}
\value{
\code{f.psi} returns a numeric value, which is the computed model comparison statistic.
}
\references{
Zhu, Q. (2020) "On improved confidence intervals for parameters of discrete distributions." PhD dissertation, University of Iowa.
}
\author{
Qiansheng Zhu
}
\seealso{
\code{\link{diff_Xsq_nr}}, \code{\link{nested_Xsq_nr}}, \code{\link{diff_Gsq_nr}}, \code{\link{nested_Gsq_nr}}, \code{\link{diff_PD_nr}}, \code{\link{nested_PD_nr}}, \code{\link{diff_Xsq_robust}}, \code{\link{nested_Xsq_robust}}, \code{\link{diff_Gsq_robust}}, \code{\link{nested_Gsq_robust}}, \code{\link{diff_PD_robust}}, \code{\link{nested_PD_robust}}, \code{\link{ci.table}}
}
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
