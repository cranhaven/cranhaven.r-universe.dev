 \name{conjugatebs}
\alias{conjugatebs}
\title{Computing the Bayesian estimators of the Birnbaum-Saunders (BS) distribution.}
\description{Computing the Bayesian estimators of the BS distribution using conjugate prior, that is,
conjugate and reference priors. The probability density function of generalized inverse Gaussian (GIG) distribution is given by \cite{good1953population}
\deqn{
f_{{GIG}}(x|\lambda,\chi,\psi)=\frac{1}{2{K}_{\lambda}(\sqrt{\psi \chi})}\Bigl(\frac{\psi}{\lambda}\Bigr)^{\lambda/2}x^{\lambda-1}\exp\biggl\{-\frac{\chi}{2x}-\frac{\psi x}{2}\biggr\},
}
where \eqn{x>0}, \eqn{-\infty<\lambda <+\infty}, \eqn{\psi>0}, and \eqn{\chi>0} are parameters of this family. The pdf of a inverse gamma (IG) distribution denoted as \eqn{{IG}(\gamma,\theta)} is given by
\deqn{f_{{IG}}(x|\gamma,\theta)=\frac{\theta^{\gamma} x^{-\gamma-1}}{\Gamma(\gamma)}\exp\left\{-\frac{\theta}{x}\right\},}
where \eqn{x>0}, \eqn{\gamma>0}, and \eqn{\theta>0} are the shape and scale parameters, respectively.
}
\usage{
conjugatebs(x,gamma0=1,theta0=1,lambda0=0.001,chi0=0.001,psi0=0.001,CI=0.95,M0=800,M=1000)
}
\arguments{
\item{x}{Vector of observations.}
\item{gamma0}{The first hyperparameter of the IG conjugate prior.}
\item{theta0}{The second hyperparameter of the IG conjugate prior.}
\item{lambda0}{The first hyperparameter of the GIG conjugate prior.}
\item{chi0}{The second hyperparameter of the GIG conjugate prior.}
\item{psi0}{The third hyperparameter of the GIG conjugate prior.}
\item{CI}{Confidence level for constructing percentile and asymptotic confidence intervals. That is 0.95 by default.}
\item{M0}{The number of sampler runs considered as burn-in.}
\item{M}{The number of total sampler runs.}
}
\value{A list including summary statistics of a Gibbs sampler for Bayesian inference including point estimation for the parameter, its standard error, and the corresponding \eqn{100(1-\alpha)\%} credible interval, goodness-of-fit measures, asymptotic \eqn{100(1-\alpha)\%} confidence interval (CI) and corresponding standard errors, and Fisher information matix.
}
\references{
I. J. Good 1953. The population frequencies of species and the estimation of population parameters. \emph{Biometrika}, 40(3-4):237-264.
}
\author{Mahdi Teimouri}
\examples{
data(fatigue)
x <- fatigue
conjugatebs(x,gamma0=1,theta0=1,lambda0=0.001,chi0=0.001,psi0=0.001,CI=0.95,M0=800,M=1000)
}
