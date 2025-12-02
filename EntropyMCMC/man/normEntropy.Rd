\name{normEntropy}
\alias{normEntropy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Theoretical value of the entropy for the multivariate gaussian}
\description{This function computes the entropy
\eqn{E_{f} [\log(f)]} of the density of the multivariate gaussian,
with parameters in a list, as it is the case for 
MCMC target density parameters. This function is used
mostly for benchmarking entropy estimation performed by the package
(using, e.g., the iid algorithm \code{IID_norm}).
}
\usage{
normEntropy(target_param)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target_param}{A list of two elements: the mean \code{target_param$mean}
  and the covariance matrix \code{target_param$v}.}
}


\value{The entropy of the Gaussian with these parameters.}


\author{Didier Chauveau.}

\examples{
d=2 # model dimension
mu=rep(0,d); v = diag(d) # mean and variance
target_param = list(mean=mu, v=v) # parameters
normEntropy(target_param) # the entropy
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file} % __ONLY ONE__ keyword per line
