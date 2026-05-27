\name{rEB.proc}
\alias{rEB.proc}
\alias{LASER.rEB}
\alias{LP.post.conv}

\title{
Relevance-Integrated Empirical Bayes Inference
}
\description{
Performs custom-tailored empirical Bayes inference via LASERs.
}
\usage{
rEB.proc(X, z, X.target, z.target, m = c(4, 6), nbag = NULL, centering = TRUE,
	lp.reg.method = "lm", coef.smooth = "BIC", nsample = min(length(z),2000),
	theta.set.prior = NULL, theta.set.post = NULL, LP.type = "L2",
	g.method = "DL", sd0 = NULL, m.EB = 8, parallel = FALSE,
	avg.method = "mean", post.curve = "HPD", post.alpha = 0.8,
	color = "red", ...)
}
\arguments{
  \item{X}{A \eqn{n}-by-\eqn{d} matrix of covariate values}
  \item{z}{A length \eqn{n} vector containing observations of target random variable.}
  \item{X.target}{A length \eqn{d} vector providing the set of covariates for the target case. }
  \item{z.target}{the target \eqn{z} to investigate}
  \item{m}{An ordered pair. First number indicates how many LP-nonparametric basis to construct for each \eqn{X}, second number indicates how many to construct for \eqn{z}.}
  \item{nbag}{Number of bags of parametric bootstrapped samples to use, set to \code{NULL} to disable.}
  \item{centering}{Whether to perform regression-adjustment to center the data, default is TRUE.}
  \item{lp.reg.method}{Method for estimating the relevance function and its conditional LP-Fourier coefficients. We currently support thee options: lm (inbuilt with subset selection), glmnet, and knn.}
  \item{coef.smooth}{Specifies the method to use for LP coefficient smoothing (AIC or BIC). Uses BIC by default.}
  \item{nsample}{Number of relevance samples generated for the target case.}
  \item{theta.set.prior}{This indicates the set of grid points to compute prior density.}
  \item{theta.set.post}{This indicates the set of grid points to compute posterior density.}
  \item{LP.type}{User selects either "L2" for LP-orthogonal series representation of relevance density function \eqn{d} or "MaxEnt" for the maximum entropy representation. Default is L2.}
  \item{g.method}{Suggested method for finding parameter estimates \eqn{\hat{\mu}} and \eqn{\hat{\tau}^2} for normal prior: "DL" uses Dersimonian and Lard technique; "SJ" uses Sidik-Jonkman; 'REML' uses restricted maximum likelihood; and "MoM" uses a method of moments technique. }
  \item{sd0}{Fixed standard deviation for \eqn{z|\theta}. Default is NULL, the standard error will be calculated from data.}
  \item{m.EB}{The truncation point reflecting the concentration of true nonparametric prior density \eqn{\pi} around known prior distribution \eqn{g}}
  \item{parallel}{Use parallel computing for obtaining the relevance samples, mainly used for very huge \code{nsample}, default if FALSE.}
  \item{avg.method}{For parametric bootstrapping, this specifies how the results from different bags are aggregated. ("\code{mean}" or "\code{median}".)}
  \item{post.curve}{For plotting, this specifies what to show on posterior curve. "\code{HPD}" provides HPD interval, "\code{band}" gives confidence band.}
  \item{post.alpha}{Confidence level to use when plotting posterior confidence band, or the alpha level for HPD interval.}
  \item{color}{The color of the plots.}
  \item{...}{Extra parameters to pass to other functions. Currently only supports the arguments for \code{knn()}.}
}
\value{
A list containing the following items:
	\item{result}{Contains relevant empirical Bayes prior and posterior results.}
		\item{sd0}{Initial estimate for null standard errors.}
		\item{prior}{Relevant empirical Bayes prior results.}
		\item{$g.par}{Parameters for \eqn{g=N(\mu,\tau^2)}.}
		\item{$g.method}{Method used for finding the parameter estimates \eqn{\hat{\mu}} and \eqn{\hat{\tau}^2} for \eqn{g}.}
		\item{$LP.coef}{Reports the LP-coefficients of the relevance function \eqn{d_x(x)}.}

		\item{posterior}{Relevant empirical Bayes posterior results. }
		\item{$post.mode}{Posterior mode for \eqn{\pi(\theta|z,\boldsymbol{x})}.}
		\item{$post.mean}{Posterior mean for \eqn{\pi(\theta|z,\boldsymbol{x})}.}
		\item{$post.mean.sd}{Standard error for the posterior mean, when using parametric bootstrap.}
		\item{$HPD.interval}{The HPD interval for posterior \eqn{\pi(\theta|z,\boldsymbol{x})}.}
		\item{$post.alpha}{same as input \code{post.alpha}.}

	\item{plots}{The plots for prior and posterior density.}
}
\references{
Mukhopadhyay, S., and Wang, K (2021) "On The Problem of Relevance in Statistical Inference". <arXiv:2004.09588>
}
\author{
\packageAuthor{LPRelevance}

Maintainer: \packageMaintainer{LPRelevance}
}

\examples{
\donttest{
data(funnel)
X<-funnel$x
z<-funnel$z
X.target=60
z.target=4.49
rEB.out<-rEB.proc(X,z,X.target,z.target,m=c(4,8),
	theta.set.prior=seq(-2,2,length.out=200),
	theta.set.post=seq(-2,5,length.out=200),
	centering=TRUE,m.EB=6,nsample=1000)
rEB.out$plots$rEB.post
rEB.out$plots$rEB.prior
}
}
\keyword{ Main Functions }
