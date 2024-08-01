\name{AROC.sp}
\alias{AROC.sp}
\title{
Semiparametric frequentist inference of the covariate-adjusted ROC curve (AROC).
}
\description{
Estimates the covariate-adjusted ROC curve (AROC) using the semiparametric approach proposed by Janes and Pepe (2009).
}
\usage{
AROC.sp(formula.healthy, group, tag.healthy, data, 
	est.surv.h = c("normal", "empirical"), p = seq(0, 1, l = 101), B = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula.healthy}{A \code{\link[stats]{formula}} object specifying the location regression model to be fitted in healthy population (see Details).}
  \item{group}{A character string with the name of the variable that distinguishes healthy from diseased individuals.}
  \item{tag.healthy}{The value codifying the healthy individuals in the variable \code{group}.}
  \item{data}{Data frame representing the data and containing all needed variables.}
  \item{est.surv.h}{A character string. It indicates how the conditional distribution function of the diagnostic test in healthy population is estimated. Options are "normal" and "empirical" (see Details). The default is "normal".}
  \item{p}{Set of false positive fractions (FPF) at which to estimate the covariate-adjusted ROC curve.}
  \item{B}{An integer value specifying the number of bootstrap resamples for the construction of the confidence intervals. By default 1000.}
}
\details{
Estimates the covariate-adjusted ROC curve (AROC) defined as

\deqn{AROC\left(t\right) = Pr\{1 - F_{\bar{D}}(Y_D | \mathbf{X}_{D}) \leq t\},}

where \eqn{F_{\bar{D}}(\cdot|\mathbf{X}_{\bar{D}})} denotes the conditional distribution function for \eqn{Y_{\bar{D}}} conditional on the vector of covariates \eqn{\mathbf{X}_{\bar{D}}}. In particular, the method implemented in this function estimates the outer probability empirically (see Janes and Pepe, 2008) and \eqn{F_{\bar{D}}(\cdot|\mathbf{X}_{\bar{D}})} is estimated assuming a semiparametric location regression model for \eqn{Y_{\bar{D}}}, i.e., 

\deqn{Y_{\bar{D}} = \mathbf{X}_{\bar{D}}^{T}\mathbf{\beta}_{\bar{D}} + \sigma_{\bar{D}}\varepsilon_{\bar{D}},}

such that, for a random sample \eqn{\{(\mathbf{x}_{\bar{D}i})\}_{i=1}^{n_{\bar{D}}}} from the healthy population, we have

\deqn{F_{\bar{D}}(y | \mathbf{X}_{\bar{D}}=\mathbf{x}_{\bar{D}i}) = F_{\bar{D}}\left(\frac{y-\mathbf{x}_{\bar{D}i}^{T}\mathbf{\beta}_{\bar{D}}}{\sigma_{\bar{D}}}\right),}

where \eqn{F_{\bar{D}}} is the distribution function of \eqn{\varepsilon_{\bar{D}}}. In line with the assumptions made about the distribution of \eqn{\varepsilon_{\bar{D}}}, estimators will be referred to as: (a) "normal", where Gaussian error is assumed, i.e., \eqn{F_{\bar{D}}(y) = \Phi(y)}; and, (b) "empirical", where no assumption is made about the distribution (in this case, the distribution function \eqn{F_{\bar{D}}} is empirically estimated on the basis of standardised residuals).
}
\value{
As a result, the function provides a list with the following components:
\item{call}{The matched call.}
\item{p}{Set of false positive fractions (FPF) at which the pooled ROC curve has been estimated}
\item{ROC}{Estimated covariate-adjusted ROC curve (AROC), and 95\% pointwise confidence intervals (if required)}
\item{AUC}{Estimated area under the covariate-adjusted ROC curve (AAUC), and 95\% pointwise confidence intervals (if required).}
\item{fit.h}{Object of class \code{\link[stats]{lm}} with the fitted regression model in the healthy population.}
\item{est.surv.h}{The value of the argument \code{est.surv.h} used in the call.}
}
\references{
Janes, H., and Pepe, M.S. (2009). Adjusting for covariate effects on classification accuracy using the covariate-adjusted receiver operating characteristic curve. Biometrika, 96(2), 371 - 382.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{AROC.bnp}}, \code{\link{AROC.bsp}}, \code{\link{AROC.sp}}, \code{\link{AROC.kernel}}, \code{\link{pooledROC.BB}} or \code{\link{pooledROC.emp}}.
}
\examples{
library(AROC)
data(psa)
# Select the last measurement
newpsa <- psa[!duplicated(psa$id, fromLast = TRUE),]

# Log-transform the biomarker
newpsa$l_marker1 <- log(newpsa$marker1)
\donttest{
m3 <- AROC.sp(formula.healthy = l_marker1 ~ age,
group = "status", tag.healthy = 0, data = newpsa,
p = seq(0,1,l=101), B = 500)

summary(m3)

plot(m3)
}
\dontshow{
m3 <- AROC.sp(formula.healthy = l_marker1 ~ age,
group = "status", tag.healthy = 0, data = newpsa,
p = seq(0,1,l=101), B = 0)

summary(m3)

plot(m3)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
