\name{rEB.Finite.Bayes}
\alias{rEB.Finite.Bayes}

\title{
Relevance-Integrated Finite Bayes.
}
\description{
Performs custom-tailored Finite Bayes inference via LASERs.
}
\usage{
rEB.Finite.Bayes(X,z,X.target,z.target,m=c(4,6),m.EB=8, B=10, centering=TRUE,
            nsample=min(1000,length(z)), g.method='DL',LP.type='L2',  sd0=NULL,
            theta.set.prior=seq(-2.5*sd(z),2.5*sd(z),length.out=500),
            theta.set.post=seq(z.target-2.5*sd(z),z.target+2.5*sd(z),length.out=500),
            post.alpha=0.8,  plot=TRUE, ...)
}
\arguments{
  \item{X}{A \eqn{n}-by-\eqn{d} matrix of covariate values}
  \item{z}{A length \eqn{n} vector containing observations of target random variable.}
  \item{X.target}{A length \eqn{d} vector providing the set of covariates for the target case. }
  \item{z.target}{the target \eqn{z} to investigate}
  \item{m}{An ordered pair. First number indicates how many LP-nonparametric basis to construct for each \eqn{X}, second number indicates how many to construct for \eqn{z}.}
  \item{m.EB}{The truncation point reflecting the concentration of true nonparametric prior density \eqn{\pi} around known prior distribution \eqn{g}}
  \item{B}{Number of bags of bootstrap samples for Finite Bayes.}
  \item{centering}{Whether to perform regression-adjustment to center the data, default is TRUE.}
  \item{nsample}{Number of relevance samples generated for the target case.}
  \item{g.method}{Suggested method for finding parameter estimates \eqn{\hat{\mu}} and \eqn{\hat{\tau}^2} for normal prior: "DL" uses Dersimonian and Lard technique; "SJ" uses Sidik-Jonkman; 'REML' uses restricted maximum likelihood; and "MoM" uses a method of moments technique. }
  \item{LP.type}{User selects either "L2" for LP-orthogonal series representation of relevance density function \eqn{d} or "MaxEnt" for the maximum entropy representation. Default is L2.}
  \item{sd0}{Fixed standard deviation for \eqn{z|\theta}. Default is NULL, the standard error will be calculated from data.}
  \item{theta.set.prior}{This indicates the set of grid points to compute prior density.}
  \item{theta.set.post}{This indicates the set of grid points to compute posterior density.}
  \item{post.alpha}{The alpha level for posterior HPD interval.}
  \item{plot}{Whether to display plots for prior and posterior of Relevance Finite Bayes.}
  \item{...}{Extra parameters to pass to LASER function.}
}
\value{
A list containing the following items:
		\item{prior}{Relevant Finite Bayes prior results.}
		\item{$prior.fit}{Prior density curve estimation.}

		\item{posterior}{Relevant empirical Bayes posterior results. }
		\item{$post.fit}{Posterior density curve estimation.}
		\item{$post.mode}{Posterior mode for \eqn{\pi(\theta|z,\boldsymbol{x})}.}
		\item{$post.mean}{Posterior mean for \eqn{\pi(\theta|z,\boldsymbol{x})}.}
		\item{$post.mean.sd}{Standard error for the posterior mean.}
		\item{$HPD.interval}{The HPD interval for posterior \eqn{\pi(\theta|z,\boldsymbol{x})}.}

		\item{g.par}{Parameters for \eqn{g=N(\mu,\tau^2)}.}
		\item{LP.coef}{Reports the LP-coefficients of the relevance function \eqn{d_x(x)}.}
		\item{sd0}{Initial estimate for null standard errors.}

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
X.target=30
z.target=4.49
rFB.out=rEB.Finite.Bayes(X,z,X.target,z.target,B=5,nsample=1000,m=c(4,8),m.EB=8,
                      theta.set.prior=seq(-4,4,length.out=500),
                      theta.set.post=seq(0,5,length.out=500),cred.interval=0.8,parallel=FALSE)
rFB.out$plots$prior
rFB.out$plots$post
}
}
\keyword{ Main Functions }
