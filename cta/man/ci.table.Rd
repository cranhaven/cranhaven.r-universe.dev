\name{ci.table}
\alias{ci.table}
\title{Test-Inversion CIs for Estimands in Contingency Tables}
\description{
  Constructs test-inversion approximate confidence intervals (CIs) for
  estimands in contingency tables subject to equality constraints.
  Test statistics include Wald-type statistics, and difference and
  nested versions of power-divergence statistics. This program can also compute
  test-inversion approximate confidence intervals for estimands in
  contingency tables without additionally imposed equality constraints,
  by setting the constraint function \code{h.fct = 0}.
}
\usage{
ci.table(y, h.fct = 0, h.mean = FALSE, S.fct, S.mean = FALSE, S.P = FALSE,
         S.space.H0 = NULL, method = "all", cc = 0.95, pdlambda = 2/3,
         trans.g = NULL, trans.g.epsilon = 0, trans.g.inv = NULL,
         strata = rep(1, length(y)), fixed.strata = "all", delta = 0.5,
         max.iter = 50, tol = 1e-2, tol.psi = 1e-4, adj.epsilon = 0.03,
         iter.robust.max = 30, iter.robust.eff = 10, check.homog.tol = 1e-9,
         check.zero.order.homog.tol = 1e-9, max.mph.iter = 1000, step = 1,
         change.step.after = 0.25 * max.mph.iter, y.eps = 0, iter.orig = 5,
         norm.diff.conv = 1e-6, norm.score.conv = 1e-6,
         max.score.diff.iter = 10, h0.fct.deriv = NULL,
         S0.fct.deriv = NULL, trans.g.deriv = NULL, plot.CIs = TRUE)
}
\arguments{
  \item{y}{Observed table counts in the contingency table(s), in vector form.}
  \item{h.fct}{The imposed equality constraint(s). Note that sampling constraints are not included in \code{h.fct}, and the imposed equality constraints should be non-redundant.

If \code{h.mean = FALSE} (default), \eqn{h(p)} should be the input, where \eqn{p} is the vector of data model probabilities, or it can be called the vector of table probabilities; If \code{h.mean = TRUE}, \eqn{h(m)} should be the input, where \eqn{m} is the vector of expected table counts, i.e. \eqn{m = E(Y)}. In the case of \eqn{h(m)} being the input, the function \eqn{h(\cdot)} should be \eqn{Z} homogeneous, where \eqn{Z} is the population matrix. For the definition of \eqn{Z} homogeneity and the population matrix, see Lang (2004). Note that if there is no imposed equality constraint, we should input \code{h.fct = 0} (real number \code{0}). Please do not specify \code{h.fct} as a zero function in this case. On the contrary, if there is (are) imposed equality constraint(s), please specify \code{h.fct} as an \R function. Another important note is that if there are multiple imposed equality constraints, please use \code{rbind()}, not \code{c()}, to concatenate the imposed equality constraints into a column vector.

By default, \code{h.fct = 0}.}
  \item{h.mean}{Logical argument, \code{TRUE} or \code{FALSE}. If \code{h.mean = FALSE} (default), the input \code{h.fct} is treated as a function of \eqn{p}; If \code{h.mean = TRUE}, the input \code{h.fct} is treated as a function of \eqn{m}.}
  \item{S.fct}{Parameter or estimand of interest. It should be an \R function, which returns a real number. i.e. \eqn{S(\cdot)} is a real-valued function. If \code{S.mean = FALSE} and \code{S.P = FALSE} (default), \eqn{S(p)} should be the input; If \code{S.mean = TRUE}, \eqn{S(m)} should be the input; If \code{S.P = TRUE}, \eqn{S(P)} should be the input, where \eqn{P} is the vector of joint probabilities, or it can be called the vector of pre-data probabilities. In the case of \eqn{S(m)} or \eqn{S(P)} being the input, the function \eqn{S(\cdot)} should be zero-order \eqn{Z} homogeneous, then \eqn{S(P)} is \eqn{Z} estimable with \eqn{S(P) = S(m)}. In addition, when we are in the process of computing test-inversion confidence intervals other than Wald intervals, we have to fit several models and obtain constrained MLEs of expected table counts. These models have equality constraints \eqn{h_{0}^{*}(m) = 0}, where \eqn{h_{0}^{*}(m) = (h_{0}'(m), S_{0}(m) - \psi, samp_{0}'(m))'}. Here \eqn{h_{0}(m) = 0} is (are) the imposed equality constraint(s), written in terms of \eqn{m}; \eqn{S_{0}(m) - \psi = 0} means that the estimand of interest is equal to \eqn{\psi}; \eqn{samp_{0}(m) = 0} is (are) the sampling constraint(s), written in terms of \eqn{m}. Restriction of \eqn{S(m)} [or \eqn{S(P)}] to zero-order \eqn{Z} homogeneity guarantees the \eqn{Z} homogeneity of \eqn{h_{0}^{*}(m)}.}
  \item{S.mean, S.P}{Logical argument, \code{TRUE} or \code{FALSE}. If \code{S.mean = FALSE} and \code{S.P = FALSE} (default), the input \code{S.fct} is treated as a function of \eqn{p}; If \code{S.mean = TRUE}, the input \code{S.fct} is treated as a function of \eqn{m}; If \code{S.P = TRUE}, the input \code{S.fct} is treated as a function of \eqn{P}.}
  \item{S.space.H0}{Restricted estimand space of \eqn{S(\cdot)} under \eqn{H_{0}}, i.e. subject to the imposed equality constraints along with sampling constraints. If \code{S.space.H0} is not specified or the input \code{S.space.H0 = NULL}, the restricted estimand space is treated as \eqn{(-\infty, \infty)}, i.e. the whole real number line. If \code{S.space.H0} is specified, it can either be input as a vector of length of an even number, or be input in class \code{Intervals_full \{intervals\}}. As an example, if the restricted estimand space is \eqn{(-\infty, -1] \cup [1, \infty)}, then the input \code{S.space.H0} could be \code{c(-Inf, -1, 1, Inf)}, or
  \code{Intervals_full(matrix(c(-Inf, -1, 1, Inf), ncol = 2, byrow = TRUE), closed = matrix(c(FALSE, TRUE, TRUE, FALSE), ncol = 2, byrow = TRUE), type = "R")}. It is strongly recommended that \code{S.space.H0} be specified, as it will improve the accuracy and (possibly) speed in interval estimation. However, it is often difficult to have an idea of the restricted estimand space exactly. In this scenario, specification of one (or several) possibly larger interval(s) that cover(s) the exact restricted estimand space is also helpful.}
  \item{method}{The test statistic(s) in constructing the test-inversion approximate confidence interval(s). There are eight different test statistics, and the user is allowed to choose any number of the test statistics out of the eight. The eight test statistics are listed as follows: \code{"Wald"}, \code{"trans.Wald"} (need specification of the transformation \eqn{g}), \code{"diff.Xsq"}, \code{"nested.Xsq"}, \code{"diff.Gsq"} (same as \code{"PL"} or \code{"LR"}), \code{"nested.Gsq"}, \code{"diff.PD"}, \code{"nested.PD"} (need specification of the power-divergence index parameter \eqn{\lambda}). If the input \code{method = "all"} (default), all test statistics will be employed to compute confidence intervals.}
  \item{cc}{Confidence coefficient, or the nominal level of the confidence interval.}
  \item{pdlambda}{The index parameter \eqn{\lambda} in the power-divergence statistic.}
  \item{trans.g}{The transformation \eqn{g} used in the transformed Wald confidence interval. First, we construct a confidence interval for \eqn{g(S(\cdot))}, then we back-transform, i.e. apply \eqn{g^{-1}} to the endpoints in order to obtain a confidence interval for \eqn{S(\cdot)}. There are several built-in options for the transformation: \code{"Fisher's z"}, \code{"log"}, \code{"-log"} (same as \code{"negative log"}), and \code{"[A, B]"}. \code{"[A, B]"} refers to the reparameterization trick as stated in the Discussion part of Lang (2008). The user is also allowed to input their own choice of \code{trans.g}. Ordinarily, the transformation \eqn{g} should be a bijection. Ideally, \eqn{g} should be smooth, strictly monotonically increasing, and "to parameterize away the boundary" (Lang, 2008).}
  \item{trans.g.epsilon}{The small \eqn{\epsilon} adjustment included in the transformation \eqn{g}. For example, the \code{"[A, B]"} transformation \eqn{g} with the small \eqn{\epsilon} is \deqn{g(x) = \log(x - A + \epsilon) - \log(B + \epsilon - x).}
By default, \code{trans.g.epsilon = 0}.}
  \item{trans.g.inv}{\eqn{g^{-1}} function used in back-transformation step in construction of the transformed Wald confidence interval. If \code{trans.g} is any one of the built-in options, then \code{trans.g.inv} is automatically specified accordingly.}
  \item{strata}{Vector of the same length as \code{y} that gives the stratum membership identifier. By default, \code{strata = rep(1, length(y))} refers to the single stratum (non-stratified) setting. As another example, \code{strata = c(1,1,2,2)} means that the first and second table cells belong to the first stratum, and the third and fourth table cells belong to the second stratum.}
  \item{fixed.strata}{The object that gives information on which stratum (strata) has (have) fixed sample sizes. It can equal one of the keywords, \code{fixed.strata = "all"} or \code{fixed.strata = "none"}, or it can be a vector of stratum membership identifiers, e.g. \code{fixed.strata = c(1,3)} or \code{fixed.strata = c("pop1", "pop5")}.}
  \item{delta}{The constant \eqn{\delta} that is in expressions of the moving critical values within each sliding quadratic step. By default, \code{delta = 0.5}.}
  \item{max.iter}{One of the stopping criteria. It is the maximum number of iterations in the sliding quadratic root-finding algorithm for searching the two roots to the test-inversion equation.}
  \item{tol}{One of the stopping criteria. In solving for the roots to the test-inversion equation, if the test statistic for testing \eqn{H_{0}(\psi): S_{0}(m) = \psi} vs. not \eqn{H_{0}(\psi)} under the general hypothesis \eqn{H_{0}: (h_{0}'(m), samp_{0}'(m))' = 0}, for a certain \eqn{\psi}, is within \code{tol} of the critical value, then we stop the iterations, and this current \eqn{\psi} is treated as one root. Note that since we are constructing approximate (contrary to exact) confidence intervals based on the asymptotic distribution under the null hypothesis, \code{tol} need not be too small.}
  \item{tol.psi}{One of the stopping criteria. In solving for the roots to the test-inversion equation, if the two \eqn{\psi}'s that are in nearby iterates in the corresponding tests \eqn{H_{0}(\psi)} vs. not \eqn{H_{0}(\psi)} under the general hypothesis \eqn{H_{0}}, are less than \code{tol.psi} apart in distance, then we stop the iterations, and the current \eqn{\psi} is treated as one root. Note that we should specify \code{tol.psi} to be sufficiently small (compared with the size of the restricted estimand space) so that the iterations are to be terminated mainly because of closeness of the test statistic to the critical value.}
  \item{adj.epsilon, iter.robust.max, iter.robust.eff}{The parameters used in the robustifying procedure. First, we attempt to construct confidence intervals based on the original data \code{y}, but an error might occur during this process. The reason for occurrence of the error might be the non-existence of the constrained MLE subject to \eqn{H_{0}}, or it might be because of the fact that the \eqn{\psi} in the hypothesis test \eqn{H_{0}(\psi)} vs. not \eqn{H_{0}(\psi)} is, on some scale, too far away from \eqn{\widehat{\psi}} which is the constrained MLE of the estimand subject to \eqn{H_{0}}, although this \eqn{\psi} is still within the restricted estimand space. If an error, or non-convergence issue occurs, then the program will go through the robustifying procedure, with the goal of reporting a confidence interval anyway, even in the most extreme configuration and/or with the most "extreme" data.

In the robustifying procedure, we adjust the original data \code{y} by adding \code{1 * adj.epsilon} to each original table count, and compute the confidence interval based on the adjusted data \code{y + 1 * adj.epsilon}. Note, however, that even the adjusted data may lead to non-convergence issue sometimes. We also adjust the original data by adding \code{2 * adj.epsilon}, \eqn{\ldots}, \code{iter.robust.max * adj.epsilon}, and compute confidence intervals based on these adjusted data, respectively. For computing purposes, as soon as \code{iter.robust.eff} confidence intervals based on the adjusted data have been successfully computed, we will not proceed further into adjustment and interval estimation based on adjusted data. Now, by exploiting the property that \deqn{\lim_{\texttt{adj.epsilon} \rightarrow 0+} CI(y + \texttt{adj.epsilon}; H_{0}) = CI(y; H_{0}) ,} we extrapolate using a polynomial fit of degree at most three based on lower and upper endpoints of the confidence intervals on adjusted data. It is advised that
\code{adj.epsilon} should not exceed \code{0.1}, but it should not be too small. By default, \code{adj.epsilon = 0.03}.}
  \item{check.homog.tol}{Round-off tolerance for \eqn{Z} homogeneity check. If the function \eqn{h(\cdot)} with respect to \eqn{m} is not \eqn{Z} homogeneous, the algorithm will stop immediately and report an error.}
  \item{check.zero.order.homog.tol}{Round-off tolerance for zero-order \eqn{Z} homogeneity check. If the function \eqn{S(\cdot)} with respect to \eqn{m} or \eqn{P} is not zero-order \eqn{Z} homogeneous, the algorithm will stop immediately and report an error.}
  \item{max.mph.iter, step, change.step.after, y.eps, iter.orig, norm.diff.conv, norm.score.conv, max.score.diff.iter}{The parameters used in \code{mph.fit}.}
  \item{h0.fct.deriv}{The \R function object that computes analytic derivative of the transpose of the constraint function \eqn{h_{0}(\cdot)} with respect to \eqn{m}. In this algorithm, if the input function \code{h.fct} is a function of \eqn{p}, then the algorithm automatically rewrites it into another function of \eqn{m}: \eqn{h(p) = h(Diag^{-1}(ZZ'm)m) = h_{0}(m)}. If the input function \code{h.fct} is a function of \eqn{m}, then we let \eqn{h_{0}(m) = h(m)}. \code{h0.fct.deriv}, if it is specified, equals \eqn{\partial h_{0}'(m) / \partial m}. Note that if \eqn{h_{0}(\cdot)} maps from \eqn{R^p} to \eqn{R^q}, i.e. there are \eqn{q} constraints, then \code{h0.fct.deriv} returns a \eqn{p}-by-\eqn{q} matrix of partial derivatives. If \code{h0.fct.deriv} is not specified or \code{h0.fct.deriv = NULL}, numerical derivatives will be used.}
  \item{S0.fct.deriv}{The \R function object that computes analytic derivative of the estimand function \eqn{S_{0}(\cdot)} with respect to \eqn{m}. In this algorithm, if the input function \code{S.fct} is a function of \eqn{p}, then the algorithm automatically rewrites it into another function of \eqn{m}: \eqn{S(p) = S(Diag^{-1}(ZZ'm)m) = S_{0}(m)}. If the input function \code{S.fct} is a function of \eqn{m}, then we let \eqn{S_{0}(m) = S(m)}. If the input function \code{S.fct} is a function of \eqn{P}, since \eqn{S(\cdot)} is required to be zero-order \eqn{Z} homogeneous, in which case \eqn{S(P) = S(m)}, we let \eqn{S_{0}(m) =
S(P)}. \code{S0.fct.deriv}, if it is specified, equals \eqn{\partial S_{0}(m) / \partial m}. It is a column vector, whose length is the same as the length of \eqn{m}. If \code{S0.fct.deriv} is not specified or \code{S0.fct.deriv = NULL}, numerical derivatives will be used.}
  \item{trans.g.deriv}{The derivative function of the transformation \eqn{g}, i.e. \eqn{d g(w) / d w}. If it is specified, it should be an \R function, even if the derivative function is a constant function.}
  \item{plot.CIs}{Logical argument, \code{TRUE} or \code{FALSE}. If \code{plot.CIs = TRUE} (default), a visual display of the computed confidence interval(s) will be created. If \code{plot.CIs = FALSE}, no plots will be created.}
}
\value{
\code{ci.table} returns a list, which includes the following objects:
  \item{result.table}{A table that displays lower and upper endpoints of the computed confidence interval(s). The length(s) of the confidence interval(s) is (are) reported in the last column.}
  \item{CIs}{An object of class \code{Intervals_full} \code{{intervals}} that includes all of the computed confidence interval(s).}
  \item{Shat}{The constrained MLE of \eqn{S(\cdot)} subject to \eqn{H_{0}}. If there is an error or non-convergence issue during the process of fitting the model subject to \eqn{H_{0}} by \code{mph.fit}, \code{Shat} is set to be \code{NA}; or if the constrained MLE does not exist, \code{Shat} is also set to be \code{NA}.}
  \item{ase.Shat}{The asymptotic standard error, i.e. ase, of the constrained MLE of \eqn{S(\cdot)} subject to \eqn{H_{0}}. If there is an error or non-convergence issue during the process of fitting the model subject to \eqn{H_{0}} by \code{mph.fit}, \code{ase.Shat} is set to be \code{NA}; or if the constrained MLE does not exist, \code{ase.Shat} is also set to be \code{NA}.}
  \item{S.space.H0}{Restricted estimand space of \eqn{S(\cdot)} under \eqn{H_{0}}. It might be different from the input \code{S.space.H0}. If the input \code{S.space.H0} is the union of at least two disjoint intervals, then the output \code{S.space.H0} displays the particular interval in which \code{Shat}, the constrained MLE of \eqn{S(\cdot)} subject to \eqn{H_{0}}, lies. If the input \code{S.space.H0} is an interval, then the output \code{S.space.H0} is the same as the input. If \code{S.space.H0} is unspecified or \code{S.space.H0 = NULL} in the input, then the output \code{S.space.H0 = NULL}.}
  \item{cc}{Confidence coefficient, or the nominal level of the confidence interval. It is the same as the \code{cc} in the input.}
  \item{method}{The test statistic(s) that is (are) actually used to construct the test-inversion approximate confidence interval(s).}
  \item{pdlambda}{The index parameter \eqn{\lambda} in the power-divergence statistic. It is the same as the \code{pdlambda} in the input.}
  \item{warnings.collection}{Includes all of the warning messages that occur during construction of the confidence
interval(s). They might be on evoking of the robustifying procedure: \code{"xxx.CI: Adjustment used. Not on original data.\n"}, or they might be on unsuccessful construction of the confidence interval(s): \code{"xxx.CI: NA.\n"}}
}
\references{
Lang, J. B. (2004) Multinomial-Poisson homogeneous models for contingency tables, \emph{Annals of Statistics}, \bold{32}, 340--383.

Lang, J. B. (2008) Score and profile likelihood confidence intervals for contingency table parameters, \emph{Statistics in Medicine}, \bold{27}, 5975--5990.

Zhu, Q. (2020) "On improved confidence intervals for parameters of discrete distributions." PhD dissertation, University of Iowa.
}
\author{Qiansheng Zhu}
\seealso{
\code{\link{mph.fit}}, \code{\link{mph.summary}}
}
\examples{
### Construct test-inversion CIs subject to equality constraints.

# I. Mice-Fungicide data: Innes et al. (1969) conducted an experiment
#    to test the possible carcinogenic effect of a fungicide Avadex on
#    four subgroups of mice. The data is reproduced as a 2-by-2-by-4
#    three-way contingency table. Within each of the four 2-by-2 two-way
#    sub-tables, there is one fixed stratum for the treated group, and
#    there is also one fixed stratum for the control group. Overall,
#    the data was collected under the product-multinomial sampling scheme.
#    We assume that the relative risks that correspond to the four 2-by-2
#    two-way sub-tables are the same, and we construct 95\% test-inversion
#    confidence intervals for this common relative risk.
#
#    For a detailed description of the Mice-Fungicide data set, see
#    Gart (1971):
#    Gart, J. J. (1971) The comparison of proportions: a review of
#    significance tests, confidence intervals and adjustments for
#    stratification. Revue de l'Institut International de Statistique,
#    39(2), pp. 148-169.

obs.y <- c(4, 12, 5, 74, 2, 14, 3, 84, 4, 14, 10, 80, 1, 14, 3, 79)

h.fct <- function(p) {
  RR_1 <- p[1] / p[3]
  RR_2 <- p[5] / p[7]
  RR_3 <- p[9] / p[11]
  RR_4 <- p[13] / p[15]
  rbind(RR_1 - RR_2, RR_1 - RR_3, RR_1 - RR_4)
}

S.fct <- function(p) {
  p[1] / p[3]
}

mice_result <- ci.table(obs.y, h.fct = h.fct, S.fct = S.fct,
                        S.space.H0 = c(0, Inf), trans.g = "log",
                        strata = rep(seq(1, 8), each = 2))

\donttest{
# II. Suppose there is a 3-by-4-by-2 three-way contingency table which
#     cross-classifies three variables: X, Y, and Z. We assign scores
#     {1,2,3}, {1,2,3,4}, and {1,2} to the variables X, Y, and Z,
#     respectively. At each level of Z, there is a 3-by-4 two-way sub-table
#     for variables X and Y, and the 3-by-4 sub-table forms a fixed
#     stratum. We assume that the Pearson's correlation coefficient between
#     X and Y when Z = 1 is the same as that when Z = 2. The observed table
#     counts are (1,2,3,4,5,6,7,8,9,10,11,12) for the 3-by-4 sub-table when
#     Z = 1, and (13,14,15,16,17,18,19,20,21,22,23,24) for the 3-by-4 sub-
#     table when Z = 2. We construct a 95\% profile likelihood confidence
#     interval for this common Pearson's correlation coefficient.

corr_freq_prob <- function(freq, score.X, score.Y) {
  # Compute the Pearson's correlation coefficient based on the vector
  # of table (frequency) counts or the vector of underlying table
  # probabilities.
  # Note that the input freq is a vector.
  c <- length(score.X)
  d <- length(score.Y)
  freq <- matrix(freq, nrow = c, ncol = d, byrow = TRUE)
  P <- freq / sum(freq)
  P.row.sum <- apply(P, 1, sum)
  P.column.sum <- apply(P, 2, sum)
  EX <- crossprod(score.X, P.row.sum)
  EY <- crossprod(score.Y, P.column.sum)
  EXsq <- crossprod(score.X^2, P.row.sum)
  EYsq <- crossprod(score.Y^2, P.column.sum)
  sdX <- sqrt(EXsq - EX^2)
  sdY <- sqrt(EYsq - EY^2)
  EXY <- 0
  for (i in seq(1, c)) {
    for (j in seq(1, d)) {
      EXY <- EXY + score.X[i] * score.Y[j] * P[i, j]
    }
  }
  Cov.X.Y <- EXY - EX * EY
  if (Cov.X.Y == 0) {
    corr <- 0
  }
  else {
    corr <- as.numeric(Cov.X.Y / (sdX * sdY))
  }
  corr
}

h.fct <- function(p) {
  corr_1 <- corr_freq_prob(p[seq(1, 12)], c(1, 2, 3), c(1, 2, 3, 4))
  corr_2 <- corr_freq_prob(p[seq(13, 24)], c(1, 2, 3), c(1, 2, 3, 4))
  corr_1 - corr_2
}

S.fct <- function(p) {
  corr_freq_prob(p[seq(1, 12)], c(1, 2, 3), c(1, 2, 3, 4))
}

corr_result <- ci.table(y = seq(1, 24), h.fct = h.fct, S.fct = S.fct,
                        S.space.H0 = c(-1, 1), method = "LR",
                        trans.g = "Fisher's z", strata = rep(c(1, 2), each = 12),
                        plot.CIs = FALSE)


# III. Crying Baby data: Gordon and Foss (1966) conducted an experiment to
#      investigate the effect of rocking on the crying of full term babies.
#      The data set can be reproduced as a 2-by-2-by-18 three-way contingency
#      table. Within each of the eighteen 2-by-2 two-way sub-tables, there is
#      one fixed stratum for the experimental group and one fixed stratum for
#      the control group. Overall, the data was collected under the product-
#      multinomial sampling scheme. We assume common odds ratios among the
#      eighteen two-way sub-tables, and we construct 95\% test-inversion
#      confidence intervals for this common odds ratio.
#
#      For a detailed description of the Crying Baby data set, see Cox (1966):
#      Cox, D. R. (1966) A simple example of a comparison involving quantal
#      data. Biometrika, 53(1-2), pp. 213-220.

obs.y <- c(0,1,5,3,0,1,4,2,0,1,4,1,1,0,5,1,0,1,1,4,0,1,5,4,0,1,3,5,0,1,
           4,4,0,1,2,3,1,0,1,8,0,1,1,5,0,1,1,8,0,1,3,5,0,1,1,4,0,1,2,4,
           0,1,1,7,1,0,2,4,0,1,3,5)
strata <- rep(seq(1, 36), each = 2)

h.fct <- function(p) {
  OR_1 <- p[1] * p[4] / (p[2] * p[3])
  OR_2 <- p[5] * p[8] / (p[6] * p[7])
  OR_3 <- p[9] * p[12] / (p[10] * p[11])
  OR_4 <- p[13] * p[16] / (p[14] * p[15])
  OR_5 <- p[17] * p[20] / (p[18] * p[19])
  OR_6 <- p[21] * p[24] / (p[22] * p[23])
  OR_7 <- p[25] * p[28] / (p[26] * p[27])
  OR_8 <- p[29] * p[32] / (p[30] * p[31])
  OR_9 <- p[33] * p[36] / (p[34] * p[35])
  OR_10 <- p[37] * p[40] / (p[38] * p[39])
  OR_11 <- p[41] * p[44] / (p[42] * p[43])
  OR_12 <- p[45] * p[48] / (p[46] * p[47])
  OR_13 <- p[49] * p[52] / (p[50] * p[51])
  OR_14 <- p[53] * p[56] / (p[54] * p[55])
  OR_15 <- p[57] * p[60] / (p[58] * p[59])
  OR_16 <- p[61] * p[64] / (p[62] * p[63])
  OR_17 <- p[65] * p[68] / (p[66] * p[67])
  OR_18 <- p[69] * p[72] / (p[70] * p[71])
  rbind(OR_1 - OR_2, OR_1 - OR_3, OR_1 - OR_4, OR_1 - OR_5, OR_1 - OR_6,
        OR_1 - OR_7, OR_1 - OR_8, OR_1 - OR_9, OR_1 - OR_10, OR_1 - OR_11,
        OR_1 - OR_12, OR_1 - OR_13, OR_1 - OR_14, OR_1 - OR_15,
        OR_1 - OR_16, OR_1 - OR_17, OR_1 - OR_18)
}

S.fct <- function(p) {
  p[1] * p[4] / (p[2] * p[3])
}

crying_baby_result <- ci.table(obs.y, h.fct = h.fct, S.fct = S.fct,
                               S.space.H0 = c(0, Inf), trans.g = "log",
                               strata = strata, fixed.strata = "all",
                               y.eps = 0.4)


# IV. Homicide data: Radelet & Pierce (1985) examined cases of 1017 homicide
#     defendants in Florida between 1973 and 1977. Both the police department
#     and prosecutors classified these cases into three mutually exclusive
#     categories: 1 = "No Felony", 2 = "Possible Felony", 3 = "Felony".
#     Three variables: police classification (P), court (i.e. prosecutors')
#     classification (C), and race of defendant/victim (R) are cross-
#     classified in a 3-by-3-by-4 three-way contingency table. The data
#     was collected based on independent Poisson sampling, and the strata
#     correspond to levels of the race combination (R).
#
#     For a detailed description of the Homicide data set, see Agresti (1984)
#     and Radelet & Pierce (1985):
#     Agresti, A. (1984). Analysis of Ordinal Categorical Data. John Wiley &
#     Sons.
#     Radelet, M. L., & Pierce, G. L. (1985). Race and prosecutorial
#     discretion in homicide cases. Law & Society Review, 19(4), pp. 587-622.
#
#     To measure agreement between police and court classifications, the four
#     estimands of interest are Cohen's unweighted kappa coefficients at four
#     levels of R, respectively. We construct 95\% test-inversion confidence
#     intervals for the estimands subject to two sets of equality constraints,
#     respectively.
#     (1) WkW and BkB have the same unweighted kappa, and BkW and WkB have
#     the same unweighted kappa.
#     (2) A "row effects" model for the conditional R-C association:
#     log mu_{ijk} = lambda + lambda_{i}^{R} + lambda_{j}^{P} + lambda_{k}^{C} +
#           lambda_{ij}^{RP} + lambda_{jk}^{PC} + tau_{i}^{RC}(w_{k} - bar{w}),
#     where race effects {tau_{i}^{RC}} that sum to zero are introduced for an
#     R-C association. The variable C is viewed as being ordinal with integer
#     monotonic scores {w_{k}}={1,2,3}.

BkW_v <- c(7, 1, 3, 0, 2, 6, 5, 5, 109)
WkW_v <- c(236, 11, 26, 7, 2, 21, 25, 4, 101)
BkB_v <- c(328, 6, 13, 7, 2, 3, 21, 1, 36)
WkB_v <- c(14, 1, 0, 6, 1, 1, 1, 0, 5)
obs.y <- c(BkW_v, WkW_v, BkB_v, WkB_v)

Unweighted.Kappa.BkW <- function(p) {
  mat.p <- matrix(p[seq(1,9)], nrow = 3, byrow = TRUE)
  Kappa(mat.p)$Unweighted[1]
}
Unweighted.Kappa.WkW <- function(p) {
  mat.p <- matrix(p[seq(10,18)], nrow = 3, byrow = TRUE)
  Kappa(mat.p)$Unweighted[1]
}
Unweighted.Kappa.BkB <- function(p) {
  mat.p <- matrix(p[seq(19,27)], nrow = 3, byrow = TRUE)
  Kappa(mat.p)$Unweighted[1]
}
Unweighted.Kappa.WkB <- function(p) {
  mat.p <- matrix(p[seq(28,36)], nrow = 3, byrow = TRUE)
  Kappa(mat.p)$Unweighted[1]
}

# Constraints (1)
library(vcd)
WkW.BkB_BkW.WkB_cons <- function(p) {
  mat.BkW <- matrix(p[seq(1,9)], nrow = 3, byrow = TRUE)
  mat.WkW <- matrix(p[seq(10,18)], nrow = 3, byrow = TRUE)
  mat.BkB <- matrix(p[seq(19,27)], nrow = 3, byrow = TRUE)
  mat.WkB <- matrix(p[seq(28,36)], nrow = 3, byrow = TRUE)
  rbind(Kappa(mat.BkW)$Unweighted[1] - Kappa(mat.WkB)$Unweighted[1],
        Kappa(mat.WkW)$Unweighted[1] - Kappa(mat.BkB)$Unweighted[1])
}
homicide_kappa_same_fit <- mph.fit(obs.y, h.fct = WkW.BkB_BkW.WkB_cons,
                                   strata = rep(c(1,2,3,4), each = 9),
                                   fixed.strata = "none")
homicide_kappa_same_fit$Gsq
pchisq(homicide_kappa_same_fit$Gsq, 2, lower.tail = FALSE)  # p-value

BkW_kappa_same <- ci.table(obs.y, h.fct = WkW.BkB_BkW.WkB_cons,
                           S.fct = Unweighted.Kappa.BkW, S.space.H0 = c(0,1),
                           strata = rep(c(1,2,3,4), each = 9),
                           fixed.strata = "none", trans.g = "[A,B]")
WkW_kappa_same <- ci.table(obs.y, h.fct = WkW.BkB_BkW.WkB_cons,
                           S.fct = Unweighted.Kappa.WkW, S.space.H0 = c(0,1),
                           strata = rep(c(1,2,3,4), each = 9),
                           fixed.strata = "none", trans.g = "[A,B]")

# Constraints (2)
X_cond_RC_v <- c(1,1,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,-1,0,0,
                 1,1,0,0,1,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,
                 1,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,
                 1,1,0,0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,-1,0,0,
                 1,1,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,
                 1,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,
                 1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,-1,0,0,
                 1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
                 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
                 1,0,1,0,1,0,1,0,0,0,1,0,0,0,1,0,0,0,0,-1,0,
                 1,0,1,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,
                 1,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,
                 1,0,1,0,0,1,1,0,0,0,0,1,0,0,0,0,1,0,0,-1,0,
                 1,0,1,0,0,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,
                 1,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,
                 1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,-1,0,
                 1,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
                 1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,
                 1,0,0,1,1,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,-1,
                 1,0,0,1,1,0,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,
                 1,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,
                 1,0,0,1,0,1,1,0,0,0,0,0,0,1,0,0,1,0,0,0,-1,
                 1,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,
                 1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,
                 1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,
                 1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
                 1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                 1,0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,0,0,1,1,1,
                 1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,
                 1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,
                 1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,1,1,1,
                 1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,
                 1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,
                 1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
                 1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
                 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1)
X_cond_RC_mat <- matrix(X_cond_RC_v, ncol = 21, byrow = TRUE)

cond_RC_HLP_fit <- mph.fit(obs.y, L.fct = "logm", L.mean = TRUE,
                           X = X_cond_RC_mat,
                           strata = rep(c(1,2,3,4), each = 9),
                           fixed.strata = "none")
mph.summary(cond_RC_HLP_fit)

library(MASS)
X_cond_RC_U <- Null(X_cond_RC_mat)
cond_RC_MPH_fit <- mph.fit(obs.y, h.fct = function(m) {t(X_cond_RC_U) \%*\% log(m)},
                           h.mean = TRUE, strata = rep(c(1,2,3,4), each = 9),
                           fixed.strata = "none")
mph.summary(cond_RC_MPH_fit)

BkW_cond_RC <- ci.table(obs.y, h.fct = function(m) {t(X_cond_RC_U) \%*\% log(m)},
                        h.mean = TRUE, S.fct = Unweighted.Kappa.BkW,
                        S.space.H0 = c(0,1), trans.g = "[A,B]",
                        strata = rep(c(1,2,3,4), each = 9), fixed.strata = "none")
WkW_cond_RC <- ci.table(obs.y, h.fct = function(m) {t(X_cond_RC_U) \%*\% log(m)},
                        h.mean = TRUE, S.fct = Unweighted.Kappa.WkW,
                        S.space.H0 = c(0,1), trans.g = "[A,B]",
                        strata = rep(c(1,2,3,4), each = 9), fixed.strata = "none")
BkB_cond_RC <- ci.table(obs.y, h.fct = function(m) {t(X_cond_RC_U) \%*\% log(m)},
                        h.mean = TRUE, S.fct = Unweighted.Kappa.BkB,
                        S.space.H0 = c(0,1), trans.g = "[A,B]",
                        strata = rep(c(1,2,3,4), each = 9), fixed.strata = "none")
WkB_cond_RC <- ci.table(obs.y, h.fct = function(m) {t(X_cond_RC_U) \%*\% log(m)},
                        h.mean = TRUE, S.fct = Unweighted.Kappa.WkB,
                        S.space.H0 = c(0,1), trans.g = "[A,B]",
                        strata = rep(c(1,2,3,4), each = 9), fixed.strata = "none")
}


### Construct test-inversion CIs, without additionally imposed constraints.

# V. Binomial success rate parameter p.
#    Model: 0 = x <- X | p ~ Bin(n = 5, p).
#    Goal: Compute approximate 90\% CIs for the success probability p.

bin_p_result <- ci.table(c(0, 5), h.fct = 0, S.fct = function(p) {p[1]},
                         S.space.H0 = c(0, 1), cc = 0.9, y.eps = 0.1)

#    Example 2.1 in Lang (2008).
#    Model: y = (39, 1) <- Y ~ mult(40, p1, p2).
#    Goal: Compute approximate 95\% CIs for the success probability p1.

bin_p_eg21_result <- ci.table(c(39,1), h.fct = 0, S.fct = function(p) {p[1]},
                              S.space.H0 = c(0,1), trans.g = "[A,B]")


# VI. Conditional probability.
#     Model: y = (0, 39, 18, 11) <- Y ~ mult(68, p1, p2, p3, p4)
#     Goal: Compute approximate 95\% CIs for the conditional probability
#           p1 / (p1 + p2).

cond_prob_result <- ci.table(c(0, 39, 18, 11), h.fct = 0,
                             S.fct = function(p) {p[1] / (p[1] + p[2])},
                             S.space.H0 = c(0, 1), y.eps = 0.1)

#     Model: y = (0, 39 // 18, 11) <- Y ~ prod mult(39, p1, p2 // 29, p3, p4).
#     That is,
#     y <- Y ~ MP(gamma, p | strata = c(1, 1, 2, 2), fixed = "all"),
#          where gamma = (39, 29)'.
#     Goal: Compute approximate 95\% CIs for p1.

cond_prob_SS_result <- ci.table(c(0, 39, 18, 11), h.fct = 0,
                                S.fct = function(p) {p[1]}, S.space.H0 = c(0, 1),
                                strata = c(1, 1, 2, 2), y.eps = 0.1)


# VII. Difference between conditional probabilities.
#      Model: y = (0, 39, 18, 11) <- Y ~ mult(68, p1, p2, p3, p4)
#      Goal: Compute approximate 95\% CIs for the difference between conditional
#            probabilities, p1 / (p1 + p2) - p3 / (p3 + p4).

diff_cond_prob_result <- ci.table(c(0, 39, 18, 11), h.fct = 0,
                                  S.fct = function(p) {p[1]/(p[1]+p[2]) - p[3]/(p[3]+p[4])},
                                  S.space.H0 = c(-1, 1), trans.g = "[A,B]")

\donttest{
# VIII. Gamma variant.
#       Example 2.3 in Lang (2008).
#       Model: y = (25, 25, 12 // 0, 1, 3)
#                   ~ prod mult(62, p11, p12, p13 // 4, p21, p22, p23).
#       Goal: Compute approximate 95\% CIs for the Gamma* parameter as
#             described in Lang (2008).

Gamma_variant_23 <- function(p) {
  p <- matrix(p, 2, 3, byrow = TRUE)
  P.case.gt.control <- (p[2, 2] + p[2, 3]) * p[1, 1] + p[2, 3] * p[1, 2]
  P.case.lt.control <- p[1, 2] * p[2, 1] + p[1, 3] * (p[2, 1] + p[2, 2])
  P.case.neq.control <- P.case.gt.control + P.case.lt.control
  P.case.gt.control / P.case.neq.control
}
Gamma_variant_result <- ci.table(c(25, 25, 12, 0, 1, 3), h.fct = 0,
                                 S.fct = Gamma_variant_23, S.space.H0 = c(0, 1),
                                 trans.g = "[A,B]", strata = c(1, 1, 1, 2, 2, 2))

### Alternative code...
gammastar.fct <- function(p) {
  nr <- nrow(p)
  nc <- ncol(p)
  probC <- 0
  probD <- 0
  for (i in 1:(nr-1)) {
    for (j in 1:(nc-1)) {
      Aij <- 0
      for (h in (i+1):nr) {
        for (k in (j+1):nc) {
          Aij <- Aij + p[h, k]
        }
      }
      probC <- probC + p[i, j] * Aij
    }
  }
  for (i in 1:(nr-1)) {
    for (j in 2:nc) {
      Aij <- 0
      for (h in (i+1):nr) {
        for (k in 1:(j-1)) {
          Aij <- Aij + p[h, k]
        }
      }
      probD <- probD + p[i, j] * Aij
    }
  }
  probC / (probC + probD)
}

Gamma_variant_23_a <- function(p) {
  p <- matrix(p, 2, 3, byrow = TRUE)
  gammastar.fct(p)
}
Gamma_variant_a_result <- ci.table(c(25, 25, 12, 0, 1, 3), h.fct = 0,
                                   S.fct = Gamma_variant_23_a,
                                   S.space.H0 = c(0, 1), trans.g = "[A,B]",
                                   strata = c(1, 1, 1, 2, 2, 2))


# IX. Global odds ratio.
#     Model: y = (25, 25, 12 // 0, 1, 3)
#                 ~ prod mult(62, p11, p12, p13 // 4, p21, p22, p23).
#     Goal: Compute approximate 95\% CIs for the first global odds ratio.

global_odds_ratio_23_11 <- function(p) {
  p <- matrix(p, 2, 3, byrow = TRUE)
  p[1, 1] * (p[2, 2] + p[2, 3]) / (p[2, 1] * (p[1, 2] + p[1, 3]))
}
global_odds_ratio_result <- ci.table(c(25, 25, 12, 0, 1, 3), h.fct = 0,
                                     S.fct = global_odds_ratio_23_11,
                                     S.space.H0 = c(0, Inf), trans.g = "log",
                                     strata = c(1, 1, 1, 2, 2, 2))


# X. Difference between product-multinomial probabilities.
#    Example 2.2 in Lang (2008).
#    Source (secondary): Agresti 2002:65
#    Early study of the death penalty in Florida (Radelet)
#    Victim Black...
#    White Defendant  0/9   received Death Penalty
#    Black Defendant  6/103 received Death Penalty
#
#    Model: y = (0, 9 // 6, 97) <- Y ~ prod mult(9, p1, p2 // 103, p3, p4).
#    Goal: Compute approximate 95\% CIs for the difference between
#          product-multinomial probabilities, p1 - p3.

diff_prod_mult_prob_result <- ci.table(c(0, 9, 6, 97), h.fct = 0,
                                       S.fct = function(p) {p[1] - p[3]},
                                       S.space.H0 = c(-1, 1),
                                       trans.g = "Fisher's z",
                                       strata = c(1, 1, 2, 2))

### Alternative (artificial) data that is even more sparse...

diff_prod_mult_prob_a_result <- ci.table(c(0, 9, 0, 97), h.fct = 0,
                                         S.fct = function(p) {p[1] - p[3]},
                                         S.space.H0 = c(-1, 1),
                                         trans.g = "Fisher's z",
                                         strata = c(1, 1, 2, 2), y.eps = 0.4)


# XI. Kappa coefficient.
#     Example 2.4 in Lang (2008).
#     Model: y = (4, 0, 0, 0, 1, 0, 0, 0, 15)
#              <- Y ~ mult(20, p11, p12, ..., p33).
#     Goal: Compute approximate 95\% CIs for the unweighted kappa coefficient.

Kappa_coeff_33 <- function(p) {
  p <- matrix(p, 3, 3, byrow = TRUE)
  s1 <- p[1, 1] + p[2, 2] + p[3, 3]
  prow <- apply(p, 1, sum)
  pcol <- apply(p, 2, sum)
  s2 <- prow[1] * pcol[1] + prow[2] * pcol[2] + prow[3] * pcol[3]
  (s1 - s2) / (1 - s2)
}
kappa_coeff_result <- ci.table(c(4, 0, 0, 0, 1, 0, 0, 0, 15), h.fct = 0,
                               S.fct = Kappa_coeff_33, S.space.H0 = c(-1, 1))
}
}
\keyword{htest}
