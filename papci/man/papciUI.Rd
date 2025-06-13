\name{papciUI}
\alias{papciUI}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{papciUI
%%  ~~function to do ... ~~
}

\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function will automatically launch the PA interactive user interface in a web browser.
The user interface can also be accessed by https://kate-yueyi-li.shinyapps.io/shiny . Neither R nor any packages are required in this online version.
}

\usage{
papciUI()
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
agreement(x = 90, y = 10, m = 100, n = 80, prev = 0.3)
agreement(x = 84, y = 0, m = 84, n = 97, prev = 0.096)
}

