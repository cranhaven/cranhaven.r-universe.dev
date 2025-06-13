\name{agreement}
\alias{agreement}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{agreement
%%  ~~function to do ... ~~
}

\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function is used to evaluate the agreements of two clinical tests with binary outcomes. It provides four statistical metrics, including positive percent agreement (PPA), negative percent agreement (NPA), positive predictive value (PPV), negative predictive value (NPV) and their corresponding confidence intervals (CIs) in different methods.
Some methods available in this package provide non-binomial-based CIs, which are more appropriate for CIs estimations of PPV and NPV when samples are not randomly selected from IU population or PPV=100\%.
}

\usage{
agreement(x, y, m, n, tb, baseline, comparator, prev, conf.level,
alternative, methods_pa, methods_pv, times, ...)
}

\arguments{
  \item{x}{Number of positive calls by both baseline and comparator tests. x is a non-negative integer.}
  \item{y}{Number of observations called negative at baseline but positive by comparator test. y is a non-negative integer.}
  \item{m}{Number of positive calls at baseline. m is a non-negative integer.}
  \item{n}{Number of negative calls at baseline. n is a non-negative integer.}
  \item{tb}{A 2-by-2 contingency table between baseline and comparator testing results. Baseline at column.}
  \item{baseline}{A vector of baseline testing results. 1 for call, 0 for non-call. The two vectors, \code{baseline} and \code{comparator}, are paired non-missing results and their length should be the same; ignored otherwise.}
  \item{comparator}{A vector of comparator testing results. 1 for call, 0 for non-call. Same requirements as for \code{baseline}.}
  \item{prev}{Disease prevalence based on baseline test in IU population. prev=Pr(baseline negative)}
  \item{conf.level}{The level of confidence to be used in the confidence interval.}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". }
  \item{methods_pa}{Which method to use to construct the CIs for PPA and NPA. Any combination of \code{c("exact", "ac", "asymptotic", "wilson", "prop.test", "bayes", "logit", "cloglog", "probit")} is allowed. Default is \code{"all"}.}
  \item{methods_pv}{Which method to use to construct the interval for PPV and NPV. Any combination of \code{c("Koopman","Katz","Neother","Gart_Nam","Bootstrap","Plug-In")} is allowed. Default is \code{"all"}.}
  \item{times}{Times of bootstraps if \code{"Bootstrap"} method is specified to construct the interval for PPV. Default is \code{"1000"}.}
  \item{...}{Additional arguments to be passed to \code{\link{binom.confint}}.}
}

\details{
The definitions of PPA, NPA, PPV and NPA in this package are
\itemize{
\item \eqn{PPA= Pr(comparator+|baseline+)}, \eqn{NPA= Pr(comparator-|baseline-)}
\item \eqn{PPV= Pr(baseline+|comparator+)}, \eqn{NPV= Pr(baseline-|comparator-)}
}

The point estimations are \eqn{x/m} for PPA and and \eqn{(n-y)/n} for NPA. By ignoring enrollment biases, PPV and NPV are estimated as \eqn{x/(x+y)} and \eqn{(n-y)/(m+n-x-y)}, respectively. When samples are not enrolled randomly or selected based on baseline results, PPV and NPV are obtained by the Bayes theorem and not binomially distributed. They are defined as
\deqn{PPV = prev*PPA / [ prev*PPA + (1-prev)*(1-NPA) ]}
\deqn{NPV = (1-prev)*NPA / [ (1-prev)*NPA + prev*(1-PPA) ]}


Nine methods are allowed for constructing the confidence interval(s) for PPA and NPA referring to \code{\link{binom.confint}}. Six methods are allowed for constructing the confidence interval(s) for PPV based on the risk-ratio \eqn{R1=(1-NPA)/PPA}.

\itemize{
  \item{Koopman (1984)}{ - derived the \eqn{(1-\alpha)}100\% CI for \eqn{R1} by using Chi-squared method.}
  \item{Katz et al.(1978)}{ - derived the \eqn{(1-\alpha)}100\% CI for \eqn{R1} by assuming that the \eqn{log(R1)} is approximately normally distributed.
  }
  \item{Noether (1957)}{ - developed the \eqn{(1-\alpha)}100\% CI for \eqn{R1} using an explicit solution.}
  \item{Gart and Nam (1988)}{ - improved Koopman's method by correcting the asymptotic skewness.}
  \item{Bootstrap}{ - derived the risk ratio CI using Bootstrap method from multiple random samples.}
  \item{Plug-In}{ - derived the 95\% CI for PPA and NPA as (PPA_l,PPA_u) and (NPA_l,NPA_u). Applied all four combinations (i.e., (PPA_l, NPA_l); (PPA_l, NPA_u); (PPA_u, NPA_l); (PPA_u, NPA_u)) into above PPV formulas by Bayes theorem, and the minimum and maximum values are determined as the lower and upper bound of 95\% CIs of PPV.}
}
Given the CIs for the risk-ratio \eqn{R1}, denoted as [\eqn{R1_l}, \eqn{R1_u}], the CIs for PPV can be directly contained by
\deqn{ [p/(p + (1-p)*R1_u),  p/(p + (1-p)*R1_l)]}
CIs of NPV can be derived in the same way.
}

\value{
A \code{list} of \code{data.frame} containing the estimated agreements (ppa, npa, ppv, npv) and the lower and upper bounds of the confidence interval for all the methods in \code{methods_pa} or \code{methods_pv}.
}

\references{
%% ~put references to the literature/web site here ~
  1. Gart John J and Nam Jun-mo (1988). Approximate interval estimation of the ratio of binomial parameters: a review and corrections for skewness, \emph{Biometrics}, 323-338.

  2. Katz DJSM, Baptista J, Azen SP and Pike MC (1978). Obtaining confidence intervals for the risk ratio in cohort studies, \emph{Biometrics}, 469-474.

  3. Koopman PAR (1984). Confidence intervals for the ratio of two binomial proportions, \emph{Biometrics}, 513-517.

  4. Noether Gottfried E (1957). Two confidence intervals for the ratio of two probabilities and some measures of effectiveness, \emph{Journal of the American Statistical Association}, \bold{52}: 36-45.
}

\author{Lei Yang, Cui Guo, Kate Li, Chang Xu (cuguo@foundationmedicine.com) }

\seealso{
\code{\link{binom.confint}} for different methods to obtain a confidence interval on the binomial probability like PPA and NPA.
}

\examples{
agreement(x = 90, y = 10, m = 100, n = 80, prev = 0.3, times=1000)
agreement(x = 84, y = 0, m = 84, n = 97, prev = 0.096, times=1000)
}

