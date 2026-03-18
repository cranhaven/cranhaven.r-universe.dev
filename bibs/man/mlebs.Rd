\name{mlebs}
\alias{mlebs}
\title{Computing the maximum likelihood (ML) estimator for the generalized Birnbaum-Saunders (GBS) distribution.}
\description{Computing the ML estimator for the GBS distribution proposed by Owen (2006) whose density function is given by
\deqn{
f_{{GBS}}(t|\alpha,\beta,\nu)=\frac{(1-\nu)t +\nu \beta}{\sqrt{2\pi}\alpha \sqrt{\beta}t^{\nu+1}} \exp\left\{-\frac{(t-\beta)^2}{2\alpha^2\beta t^{2\nu}}\right\},
}
where \eqn{t>0}. The parameters of GBS distribution are \eqn{\alpha>0}, \eqn{\beta>0}, and \eqn{0<\nu<1}. For \eqn{\nu=0.5}, the GBS distribution turns into the ordinary Birnbaum-Saunders distribution.
}
\usage{mlebs(x, start, method = "Nelder-Mead", CI = 0.95)}
\arguments{
\item{x}{Vector of observations.}
\item{start}{Vector of the initial values.}
\item{method}{The method for the numerically optimization that includes one of \code{CG},\code{Nelder-Mead}, \code{BFGS}, \code{L-BFGS-B}, and \code{SANN}.}
\item{CI}{Confidence level for constructing asymptotic confidence intervals. That is 0.95 by default.}
}
\value{A list including the ML estimator, goodness-of-fit measures, asymptotic \eqn{100(1-\alpha)\%} confidence interval (CI) and corresponding standard errors, and Fisher information matix.}

\author{Mahdi Teimouri}
\examples{
data(fatigue)
x <- fatigue
mlebs(x, start = c(1, 29), method = "Nelder-Mead", CI = 0.95)
}
