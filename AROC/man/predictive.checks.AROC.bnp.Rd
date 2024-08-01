\name{predictive.checks.AROC.bnp}
\alias{predictive.checks.AROC.bnp}
\title{
Posterior predictive checks.
}
\description{
Implements posterior predictive checks. Compares a selected test statistic computed based on the diagnostic test outcome in the nondiseased group against the same test statistics computed based on generated data from the posterior predictive distribution of the diagnostic test outcome in the nondiseased group obtained using a B-splines dependent Dirichlet process mixture model as described in Inacio de Carvalho and Rodriguez-Alvarez (2018).
}
\usage{
predictive.checks.AROC.bnp(object, 
	statistics = c("min", "max", "kurtosis", "skewness"), devnew = TRUE)
}
\arguments{
 	\item{object}{An object of class \code{AROC} as produced by \code{\link{AROC.bnp}}.}
  	\item{statistics}{Character vector. Statistics to be used for the posterior predictive checking. By default, "min", "max", "kurtosis" and "skewness"}
  	\item{devnew}{logical. If TRUE, each plot is depicted in a new graphic device.}
}
\details{
The following graphics are depicted: (1) histograms of the desired statistics computed from a number of simulated datasets drawn from the posterior predictive distribution of the diagnostic test outcome in the nondiseased group. In these plots, the estimated statistics from the observed diagnostic test outcome in the nondiseased group are also depicted. (2) Kernel density estimates computed from a number of simulated datasets drawn from the posterior predictive distribution of the diagnostic test outcome in the nondiseased group. In these plots, the kernel density estimate of the observed diagnostic test outcome in the nondiseased group is also depicted. For a detailed discussion about predictive checks, see Gabry et al. (2017).
}
\value{
As a result, the function provides a list with the following components:
\item{yrep}{Matrix. Each column corresponds to a dataset generated from the posterior predictive distribution of the diagnostic test outcome in the nondiseased group.}
\item{y0}{Numeric vector. Observed diagnostic test outcome in the nondiseased group.}
}
\references{
Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., and Gelman, A. (2017). Visualization in Bayesian workflow. arXiv preprint arXiv:1709.01449.

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
\code{\link{AROC.bnp}}
}
\examples{
library(AROC)
data(psa)
# Select the last measurement
newpsa <- psa[!duplicated(psa$id, fromLast = TRUE),]

# Log-transform the biomarker
newpsa$l_marker1 <- log(newpsa$marker1)
\donttest{
m0 <- AROC.bnp(formula.healthy = l_marker1 ~ f(age, K = 0),
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, L = 10, nsim = 5000, nburn = 1000)

predictive.checks.AROC.bnp(m0, statistics = "skewness")
}
\dontshow{
m0 <- AROC.bnp(formula.healthy = l_marker1 ~ f(age, K = 0),
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, L = 10, nsim = 500, nburn = 100)

predictive.checks.AROC.bnp(m0, statistics = "skewness")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
