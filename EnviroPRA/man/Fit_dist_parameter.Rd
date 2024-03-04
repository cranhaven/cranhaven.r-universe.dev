\name{Fit_dist_parameter}
\alias{Fit_dist_parameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Returns adjusted distribution parameters
}
\description{
Returns the distribution parameters adjusted for by maximum likelihood (mle) for the following distributions: "normal","log-normal","geometric","exponential","Poisson", "cauchy" , "logistic" and "weibull"  
  
}
\usage{
Fit_dist_parameter(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A numeric vector of length at least one containing only finite values (non-censored data)
}
}

\value{

\item{normal }{Fitted Mean and sd for a normal distribution}
\item{`log-normal` }{Fitted Meanlog and sdlog for a log-normal distribution}
\item{geometric }{Fitted prob for a geometric distribution}
\item{exponential }{Fitted rate for a exponential distribution}
\item{Poisson }{Fitted lambda for a exponential distribution}
\item{cauchy }{Fitted location and scale for a Cauchy distribution}
\item{logistic }{Fitted location and scale for a Logistic distribution}
\item{weibull }{Fitted shape and scale for a weibull distribution}

}

\author{
F. Barrio-Parra
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Function fitdistr in Library (MASS)
}
\examples{

a <- rnorm(n=100, mean =10, sd = 1) 

b <- Fit_dist_parameter(a)

# Examples of result extraction

b$normal

b$weibull
}
\keyword{ methods }