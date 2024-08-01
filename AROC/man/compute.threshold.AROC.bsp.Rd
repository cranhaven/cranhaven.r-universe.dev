\name{compute.threshold.AROC.bsp}
\alias{compute.threshold.AROC.bsp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AROC-based threshold values.
}
\description{
Estimates AROC-based threshold values using the semiparametric Bayesian normal linear regression model discussed in Inacio de Carvalho and Rodriguez-Alvarez (2018).
}
\usage{
compute.threshold.AROC.bsp(object, newdata, FPF = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{object}{An object of class \code{AROC} as produced by \code{\link{AROC.bsp}}.}
  \item{newdata}{Data frame with the covariate values at which threshold values are required.}
  \item{FPF}{Numeric vector with the FPF at which to calculate the AROC-based threshold values. Atomic values are also valid.}
}
\details{
Estimation of the covariate-adjusted ROC curve (AROC) using the semiparametric Bayesian normal linear regression model discussed in Inacio de Carvalho and Rodriguez-Alvarez (2018) involves the estimation of the conditional distribution function for the diagnostic test outcome in the healthy population

\deqn{F_{\bar{D}}(y|\mathbf{X}_{\bar{D}}) = Pr\{Y_{\bar{D}} \leq y | \mathbf{X}_{\bar{D}}\}.}

This function makes use of this estimate in order to calculate AROC-based threshold values. In particular, for a covariate value \eqn{\mathbf{x}} and a FPF = t, the AROC-based threshold value at the \eqn{s}-th posterior sample (\eqn{s = 1,\ldots,S}) is calculated as follows

\deqn{c^{(s)}_{\mathbf{x}} = \hat{F}^{-1(s)}_{\bar{D}}(1-t|\mathbf{X}_{\bar{D}} = \mathbf{x}).}

from which the posterior mean can be computed

\deqn{\hat{c}_{\mathbf{x}} = \frac{1}{S}\sum_{s = 1}^{S}c^{(s)}_{\mathbf{x}}.}
}
\value{As a result, the function provides a list with the following components:
\item{thresholds.est}{A matrix with the posterior mean of the AROC-based threshold values. The matrix has as many columns as different covariate vector values, and as many rows as different FPFs.}
\item{thresholds.ql}{A matrix with the posterior 2.5\% quantile of the AROC-based threshold values. The matrix has as many columns as different covariate vector values, and as many rows as different FPFs.}
\item{thresholds.qh}{A matrix with the posterior 97.5\% quantile of the AROC-based threshold values. The matrix has as many columns as different covariate vector values, and as many rows as different FPFs.}
}
\references{
Inacio de Carvalho, V., and Rodriguez-Alvarez, M. X. (2018). Bayesian nonparametric inference for the covariate-adjusted ROC curve. arXiv preprint arXiv:1806.00473.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{AROC.bsp}}
}
\examples{

library(AROC)
data(psa)
# Select the last measurement
newpsa <- psa[!duplicated(psa$id, fromLast = TRUE),]

# Log-transform the biomarker
newpsa$l_marker1 <- log(newpsa$marker1)
\donttest{
m1 <- AROC.bsp(formula.healthy = l_marker1 ~ age,
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, nsim = 5000, nburn = 1500)

# Compute the threshold values
FPF = c(0.1, 0.3)
newdata <- data.frame(age = seq(52, 80, l = 50))

th_bsp <- compute.threshold.AROC.bsp(m1, newdata, FPF)
names(th_bsp)
}
\dontshow{
m1 <- AROC.bsp(formula.healthy = l_marker1 ~ age,
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, nsim = 500, nburn = 150)

# Compute the threshold values
FPF = c(0.1, 0.3)
newdata <- data.frame(age = 52)

th_bsp <- compute.threshold.AROC.bsp(m1, newdata, FPF)
names(th_bsp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
