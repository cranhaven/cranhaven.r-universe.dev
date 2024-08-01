\name{AROC.kernel}
\alias{AROC.kernel}
\title{
Non parametric kernel-based estimation of the covariate-adjusted ROC curve (AROC).
}
\description{
Estimates the covariate-adjusted ROC curve (AROC) using the nonparametric kernel-based method proposed by Rodriguez-Alvarez et al. (2011). The method, as it stands now, can only deal with one continuous covariate. 
}
\usage{
AROC.kernel(marker, covariate, group, tag.healthy, data, p = seq(0, 1, l = 101), B = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{marker}{A character string with the name of the diagnostic test variable.}
  \item{covariate}{A character string with the name of the continuous covariate.}
  \item{group}{A character string with the name of the variable that distinguishes healthy from diseased individuals.}
  \item{tag.healthy}{The value codifying the healthy individuals in the variable \code{group}.}
  \item{data}{Data frame representing the data and containing all needed variables.}
  \item{p}{Set of false positive fractions (FPF) at which to estimate the covariate-adjusted ROC curve.}
  \item{B}{An integer value specifying the number of bootstrap resamples for the construction of the confidence intervals. By default 1000.}
}
\details{
Estimates the covariate-adjusted ROC curve (AROC) defined as

\deqn{AROC\left(t\right) = Pr\{1 - F_{\bar{D}}(Y_D | X_{D}) \leq t\},}

where \eqn{F_{\bar{D}}(\cdot|X_{D})} denotes the conditional distribution function for \eqn{Y_{\bar{D}}} conditional on the vector of covariates \eqn{X_{\bar{D}}}. In particular, the method implemented in this function estimates the outer probability empirically (see Janes and Pepe, 2008) and \eqn{F_{\bar{D}}(\cdot|X_{\bar{D}})} is estimated assuming a nonparametric location-scale regression model for \eqn{Y_{\bar{D}}}, i.e., 

\deqn{Y_{\bar{D}} = \mu_{\bar{D}}(X_{\bar{D}}) + \sigma_{\bar{D}}(X_{\bar{D}})\varepsilon_{\bar{D}},}

where \eqn{\mu_{\bar{D}}} is the regression funcion, \eqn{\sigma_{\bar{D}}} is the variance function, and \eqn{\varepsilon_{\bar{D}}} has zero mean, variance one, and
distribution function \eqn{F_{\bar{D}}}. As a consequence, and for a random sample \eqn{\{(x_{\bar{D}i},y_{\bar{D}i})\}_{i=1}^{n_{\bar{D}}}}

\deqn{F_{\bar{D}}(y_{\bar{D}i} | X_{\bar{D}}= x_{\bar{D}i}) = F_{\bar{D}}\left(\frac{y_{\bar{D}i}-\mu_{\bar{D}}(x_{\bar{D}i})}{\sigma_{\bar{D}}(x_{\bar{D}i})}\right).} 

Both the regression and variance functions are estimated using the Nadaraya-Watson estimator, and the bandwidth are selected using least-squares cross-validation. Implementation relies on the \code{R}-package \code{\link[np]{np}}. No assumption is made about the distribution of \eqn{\varepsilon_{\bar{D}}}, which is empirically estimated on the basis of standardised residuals. 
}
\value{
As a result, the function provides a list with the following components:
\item{call}{The matched call.}
\item{p}{Set of false positive fractions (FPF) at which the pooled ROC curve has been estimated}
\item{ROC}{Estimated covariate-adjusted ROC curve (AROC), and 95\% pointwise confidence intervals (if required)}
\item{AUC}{Estimated area under the covariate-adjusted ROC curve (AAUC), and 95\% pointwise confidence intervals (if required).}
\item{bw.mean}{An object of class \code{\link[np]{npregbw}} with the selected bandwidth for the nonparametric regression function. For further details, see  \code{R}-package \code{\link[np]{np}}.}
\item{bw.var}{An object of class \code{\link[np]{npregbw}} with the selected bandwidth for the nonparametric variance function. For further details, see  \code{R}-package \code{\link[np]{np}}.}
\item{fit.mean}{An object of class \code{\link[np]{npreg}} with the nonparametric regression function estimate. For further details, see  \code{R}-package \code{\link[np]{np}}.}
\item{fit.var}{An object of class \code{\link[np]{npreg}} with the nonparametric variance function estimate. For further details, see  \code{R}-package \code{\link[np]{np}}.}
}
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
m2 <- AROC.kernel(marker = "l_marker1", covariate = "age",
group = "status", tag.healthy = 0, data = newpsa, 
p = seq(0,1,l=101), B = 500)

summary(m2)

plot(m2)
}
\dontshow{
m2 <- AROC.kernel(marker = "l_marker1", covariate = "age",
group = "status", tag.healthy = 0, data = newpsa, 
p = seq(0,1,l=101), B = 0)

summary(m2)

plot(m2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
