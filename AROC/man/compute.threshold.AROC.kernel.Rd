\name{compute.threshold.AROC.kernel}
\alias{compute.threshold.AROC.kernel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AROC-based threshold values.
}
\description{
Estimates AROC-based threshold values using the nonparametric kernel-based method proposed by Rodriguez-Alvarez et al. (2011).
}
\usage{
compute.threshold.AROC.kernel(object, newcovariate, FPF = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{\item{object}{An object of class \code{AROC} as produced by \code{\link{AROC.kernel}}.}
  \item{newcovariate}{Numeric vector with the covariate values at which threshold values are required.}
  \item{FPF}{Numeric vector with the FPF at which to calculate the AROC-based threshold values. Atomic values are also valid.}
}
\details{
Estimation of the covariate-adjusted ROC curve (AROC) using the nonparametric kernel-based method proposed by Rodriguez-Alvarez et al. (2011) involves the estimation of the conditional distribution function for the diagnostic test outcome in the healthy population

\deqn{F_{\bar{D}}(y|X_{\bar{D}}) = Pr\{Y_{\bar{D}} \leq y | X_{\bar{D}}\}.}

This function makes use of this estimate in order to calculate AROC-based threshold values. In particular, for a covariate value \eqn{x} and a FPF = t, the AROC-based threshold value is calculated as follows

\deqn{\hat{c}_{\mathbf{x}} = \hat{F}^{-1}_{\bar{D}}(1-t|\mathbf{X}_{\bar{D}} = \mathbf{x}).}

}
\value{A matrix with the computed AROC-based threshold values. The matrix has as many columns as different covariate values, and as many rows as different FPFs.}
\references{
Hayfield, T., and Racine, J. S.(2008). Nonparametric Econometrics: The np Package. Journal of Statistical Software 27(5). URL http://www.jstatsoft.org/v27/i05/.

Inacio de Carvalho, V., and Rodriguez-Alvarez, M. X. (2018). Bayesian nonparametric inference for the covariate-adjusted ROC curve. arXiv preprint arXiv:1806.00473.

Rodriguez-Alvarez, M. X., Roca-Pardinas, J., and Cadarso-Suarez, C. (2011). ROC curve and covariates: extending induced methodology to the non-parametric framework. Statistics and Computing, 21(4), 483 - 499.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{AROC.kernel}}
}
\examples{
library(AROC)
data(psa)
# Select the last measurement
newpsa <- psa[!duplicated(psa$id, fromLast = TRUE),]

# Log-transform the biomarker
newpsa$l_marker1 <- log(newpsa$marker1)
\donttest{
m2 <- AROC.kernel(marker = "l_marker1", covariate = "age",
group = "status", tag.healthy = 0, data = newpsa, 
p = seq(0,1,l=101), B = 500)

# Compute the threshold values
cov.values <- seq(52, 80, l = 50)
FPF = c(0.1, 0.3)

th_np <- compute.threshold.AROC.kernel(m2, cov.values, FPF)
}
\dontshow{
m2 <- AROC.kernel(marker = "l_marker1", covariate = "age",
group = "status", tag.healthy = 0, data = newpsa, 
p = seq(0,1,l=101), B = 0)

# Compute the threshold values
cov.values <- 52
FPF = c(0.1, 0.3)

th_np <- compute.threshold.AROC.kernel(m2, cov.values, FPF)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
