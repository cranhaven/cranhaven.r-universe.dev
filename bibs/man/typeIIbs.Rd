\name{typeIIbs}
\alias{typeIIbs}
\title{Bayesian estimator for the Birnbaum-Saunders family under progressive type-II censoring scheme.}
\description{Estimates parameters of the Birnbaum-Saunders family in a Bayesian framework through the Metropolis-Hasting algorithm when  subjects are placed on progressive type-II censoring scheme with likelihood function
\deqn{l(\alpha,\beta|x_{1:m:n},\dots,x_{m:m:n})=\log L(\Theta) \propto C \sum_{i=1}^{m} \log f(x_{i:m:n}{{;}}|\alpha,\beta) + \sum_{i=1}^{m} R_i \log \bigl[1-F(x_{i:m:n}{{;}}|\alpha,\beta)\bigr],}
in which \eqn{F(.|\alpha,\beta)} is cumulative distribution function of the Birnbaum-Saunders family with \eqn{C=n(n-R_1-1)(n-R_1-R_2-2)\dots (n-R_1-R_2-\dots-R_{m-1}-m+1)}. The acceptance for each new sample of \eqn{\alpha} and \eqn{\beta}, respectively, becomes
\deqn{A_{\alpha}=\min \left\{1,\prod_{i=1}^{m}\frac{\bigl[1-F_{BS}(t_{i:m:n}|1/(\alpha^{new})^2,\beta)\bigr]^{R_{i}}}{\bigl[1-F_{BS}(t_{i:m:n}|1/(\alpha_{old})^2,\beta)\bigr]^{R_{i}}}\right\}},
\deqn{A_{\beta}=\min \left\{1,\prod_{i=1}^{m}\frac{\bigl[1-F_{BS}(t_{i:m:n}|\alpha,\beta^{new})\bigr]^{R_{i}}}{\bigl[1-F_{BS}(t_{i:m:n}|\alpha,\beta_{old})\bigr]^{R_{i}}}\right\}.}
}
\usage{typeIIbs(plan, M0 = 4000, M = 6000, CI = 0.95)}
\arguments{
\item{plan}{Censoring plan for progressive type-II censoring scheme. It must be given as a \code{data.frame} including: number of
item placed on the test at time zero and a vector that contains number \code{R}, of the removed alive items.}
\item{M0}{The number of sampler runs considered as burn-in.}
\item{M}{The number of total sampler runs.}
\item{CI}{Confidence or coverage level for constructing percentile confidence interval. That is 0.95 by default.}
}
\value{A list including summary statistics after burn-in point including: mean, median, standard deviation, 100(1 - \code{CI})/2 percentile, 100(1/2 + \code{CI}/2) percentile.
}
\references{
M. Teimouri and S. Nadarajah 2016. Bias corrected MLEs under progressive type-II censoring scheme, \emph{Journal of Statistical Computation and Simulation}, 86 (14), 2714-2726.

N. Balakrishnan and R. Aggarwala 2000. \emph{Progressive Censoring: Theory, Methods, and Applications}. Springer Science \eqn{\&} Business Media, New York.
}
\author{Mahdi Teimouri}
\examples{
data(plasma)
typeIIbs(plan = plasma, M0 = 100, M = 200, CI = 0.95)
}
