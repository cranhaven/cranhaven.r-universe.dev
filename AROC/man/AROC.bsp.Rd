\name{AROC.bsp}
\alias{AROC.bsp}
\title{
Semiparametric Bayesian inference of the covariate-adjusted ROC curve (AROC).
}
\description{
Estimates the covariate-adjusted ROC curve (AROC) using the semiparametric Bayesian normal linear regression model discussed in Inacio de Carvalho and Rodriguez-Alvarez (2018).
}
\usage{
AROC.bsp(formula.healthy, group, tag.healthy, data, scale = TRUE, 
  p = seq(0, 1, l = 101), paauc = paauccontrol(),
  compute.lpml = FALSE, compute.WAIC = FALSE, 
  m0, S0, nu, Psi, a = 2, b = 0.5, nsim = 5000, nburn = 1500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula.healthy}{A \code{\link[stats]{formula}} object specifying the Bayesian normal linear regression model for the estimation of the conditional distribution function for the diagnostic test outcome in the healthy population (see Details).}
  \item{group}{A character string with the name of the variable that distinguishes healthy from diseased individuals.}
  \item{tag.healthy}{The value codifying the healthy individuals in the variable \code{group}.}
  \item{data}{Data frame representing the data and containing all needed variables.}
  \item{scale}{A logical value. If TRUE the test outcomes are scaled, i.e., are divided by the standard deviation. The default is TRUE.}
  \item{p}{Set of false positive fractions (FPF) at which to estimate the covariate-adjusted ROC curve.}
  \item{compute.lpml}{A logical value. If TRUE, the log pseudo marginal likelihood (LPML, Geisser and Eddy, 1979) and the conditional predictive ordinates (CPO) are computed.}
  \item{paauc}{A list of control values to replace the default values returned by the function \code{\link{paauccontrol}}. This argument is used to indicate whether the partial area under the covariate-adjusted ROC curve (pAAUC) should be computed and at which FPF.}
  \item{compute.WAIC}{A logical value. If TRUE, the widely applicable information criterion (WAIC, Gelman et al., 2014; Watanabe, 2010) is computed.}
  \item{m0}{A numeric vector.  Hyperparameter; mean vector of the (multivariate) normal distribution for the mean of the regression coefficients. If missing, it is set to a vector of zeros of length \code{p+1} (see Details).}
  \item{S0}{A numeric matrix. Hyperprior. If missing, it is set to a diagonal matrix of dimension \code{(p+1)}x\code{(p+1)} with 100 in the diagonal (see Details).}
  \item{nu}{A numeric value. Hyperparameter; degrees of freedom of the Wishart distribution for the precision matrix of the regression coefficients. If missing, it is set to \code{p + 3} (see Details)}
  \item{Psi}{A numeric matrix. Hyperparameter; scale matrix of the Wishart distribution for the precision matrix of the regression coefficients. If missing, it is set to an identity matrix of dimension \code{(p+1)}x\code{(p+1)} (see Details).}
  \item{a}{A numeric value. Hyperparameter; shape parameter of the gamma distribution for the precision (inverse variance). The default is 2 (scaled data) (see Details).}
  \item{b}{A numeric value. Hyperparameter; rate parameter of the gamma distribution for the precision (inverse variance). The default is 0.5 (scaled data) (see Details).}
  \item{nsim}{A numeric value. Total number of Gibbs sampler iterates (including the burn-in). The default is 5000.}
  \item{nburn}{A numeric value. Number of burn-in iterations. The default is 1500.}
}
\details{
Estimates the covariate-adjusted ROC curve (AROC) defined as

\deqn{AROC\left(t\right) = Pr\{1 - F_{\bar{D}}(Y_D | \mathbf{X}_{D}) \leq t\},}

where \eqn{F_{\bar{D}}(\cdot|\mathbf{X}_{\bar{D}})} denotes the conditional distribution function for \eqn{Y_{\bar{D}}} conditional on the vector of covariates \eqn{X_{\bar{D}}}. In particular, the method implemented in this function combines a Bayesian normal linear regression model to estimate \eqn{F_{\bar{D}}(\cdot|\mathbf{X}_{\bar{D}})} and the Bayesian bootstrap (Rubin, 1981) to estimate the outside probability. More precisely, and letting \eqn{\{(\mathbf{x}_{\bar{D}i},y_{\bar{D}i})\}_{i=1}^{n_{\bar{D}}}} be a random sample from the nondiseased population

\deqn{F_{\bar{D}}(y_{\bar{D}i}|\mathbf{X}_{\bar{D}}=\mathbf{x}_{\bar{D}i}) = \Phi(y_{\bar{D}i}\mid \mathbf{x}_{\bar{D}i}^{*T}\mathbf{\beta}^{*},\sigma^2),}

where \eqn{\mathbf{x}_{\bar{D}i}^{*T} = (1, \mathbf{x}_{\bar{D}i}^{T})}, \eqn{\mathbf{\beta}^{*}\sim N_{p+1} (\mathbf{m},\mathbf{S})} and \eqn{\sigma^{-2}\sim\Gamma(a,b)}. It is assumed that \eqn{\mathbf{m} \sim N_{p+1}(\mathbf{m}_0,\mathbf{S}_0)} and \eqn{\mathbf{S}^{-1}\sim W(\nu,(\nu\Psi)^{-1})}, where \eqn{p+1} denotes the number of columns of the design matrix \eqn{\mathbf{X}_{\bar{D}}^{*}}. Here \eqn{W(\nu,(\nu\Psi)^{-1})} denotes a Wishart distribution with \eqn{\nu} degrees of freedom and expectation \eqn{\Psi^{-1}}. For a detailed description, we refer to Inacio de Carvalho and Rodriguez-Alvarez (2018). 
}
\value{
As a result, the function provides a list with the following components:
\item{call}{The matched call.}
\item{p}{Set of false positive fractions (FPF) at which the pooled ROC curve has been estimated.}
\item{ROC}{Estimated covariate-adjusted ROC curve (AROC) (posterior mean), and 95\% pointwise posterior credible band.}
\item{AUC}{Estimated area under the covariate-adjusted ROC curve (AAUC) (posterior mean), and 95\% pointwise posterior credible band.}
\item{pAUC}{If required in the call to the function, estimated partial area under the covariate-adjusted ROC curve (pAAUC) (posterior mean), and 95\% pointwise posterior credible band.}
\item{lpml}{If required, list with two components: the log pseudo marginal likelihood (LPML) and the conditional predictive ordinates (CPO).}
\item{WAIC}{If required, widely applicable information criterion (WAIC).}
\item{fit}{Results of the fitting process. It is a list with the following components: (1) \code{mm}: information needed to construct the model matrix associated with the B-splines dependent Dirichlet process mixture model. (2) \code{beta}: matrix of dimension \code{N}x\code{p+1} with the sampled regression coefficients. Here, \code{N} is the number of Gibbs sampler iterates after burn-in, and \code{p+1} the number of columns of the design matrix (see also Details). (3) \code{sd}: vector of length \code{N} with the sampled variances (see also Details).}
\item{data_model}{List with the data used in the fit: observed diagnostic test outcome and B-spline design matrices, separately for the healthy and diseased groups.}
}
\references{
Inacio de Carvalho, V., and Rodriguez-Alvarez, M. X. (2018). Bayesian nonparametric inference for the covariate-adjusted ROC curve. arXiv preprint arXiv:1806.00473.

Rubin, D. B. (1981). The Bayesian bootstrap. The Annals of Statistics, 9(1), 130-134.
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
m1 <- AROC.bsp(formula.healthy = l_marker1 ~ age,
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, nsim = 5000, nburn = 1500)

summary(m1)

plot(m1)
}
\dontshow{
m1 <- AROC.bsp(formula.healthy = l_marker1 ~ age,
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, nsim = 500, nburn = 150)

summary(m1)

plot(m1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
