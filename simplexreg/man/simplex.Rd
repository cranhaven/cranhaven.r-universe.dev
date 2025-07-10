\name{simplex}
\alias{dsimplex}
\alias{psimplex}
\alias{qsimplex}
\alias{rsimplex}
\alias{psimplex.norm}
\alias{qsimplex.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Simplex Distribution Functions
%%  ~~function to do ... ~~
}
\description{Density, cumulative distribution function, quantile function and random variable generation 
for the simplex distribution with mean equal to \code{mu} and dispersion equal to \code{sig}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dsimplex(x, mu, sig)
psimplex(q, mu, sig)
qsimplex(p, mu, sig)
rsimplex(n, mu, sig)
psimplex.norm(q, mu, sig)
qsimplex.norm(p, mu, sig)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, q}{vector of quantiles
%%     ~~Describe \code{x} here~~
}
  \item{p}{vector of probabilities}
  \item{n}{number of observations}   
  \item{mu}{vector of means
%%     ~~Describe \code{mu} here~~
}
  \item{sig}{vector of square root of dispersion parameter of simplex distribution
%%     ~~Describe \code{sig} here~~
}
}
\details{
The simplex distribution has density
\deqn{p(y) = (2\pi \sigma^2)^{-\frac{1}{2}} (y(1-y))^{-\frac{3}{2}} \exp(-\frac{1}{2\sigma^2} d(y;\mu)), y \in (0, 1)}{%
	  p(y) = (2\pi \sigma^2)^{-1/2} (y(1-y))^{-3/2} e^(-1 / (2 \sigma^2) d(y;\mu))}
where \eqn{d(y;\mu)} is a unit deviance function
\deqn{d(y;\mu) = \frac{(y-\mu)^2}{y(1-y) \mu^2 (1-\mu)^2}}{%
	  d(y;\mu) = (y - \mu)^2 / (y(1-y) \mu^2 (1-\mu)^2)} 
\eqn{\mu} is the mean of simplex distribution and \eqn{\sigma^2} the dispersion parameter. 
\code{qnorm} provides results up to about 6 digits.	  
%%  ~~ If necessary, more details than the description above ~~
}
\value{\code{dsimplex} gives density function, \code{psimplex} gives the distribution function, \code{qsimplex} gives quantile function and 
\code{rsimplex} gives random number generated from the simplex distribution. \code{psim.norm} and \code{qsimplex.norm} gives the 
renormalized distribution and quantile function.  
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Barndorff-Nielsen,  O.E. and  Jorgensen, B. (1991)
     Some parametric models on the simplex.
     \emph{ Journal of  Multivariate Analysis,} \bold{ 39:} 106--116
     
     Jorgensen, B. (1997)
   \emph{ The Theory of Dispersion Models}. London: Chapman and Hall
   
    Song, P. and Qiu, Z. and Tan, M. (2004) Modelling Heterogeneous Dispersion in 
   Marginal Models for Longitudinal Proportional Data. \emph{ Biometrical Journal,} 
   \bold{ 46:} 540--553
%% ~put references to the literature/web site here ~
}
\author{Peng Zhang and Zhenguo Qiu
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# simplex distribution function
dsimplex(seq(0.01,0.99,0.01), 0.5, 1)
psimplex(seq(0.01,0.99,0.01), 0.5, 1)
qsimplex(seq(0.01,0.99,0.01), 0.5, 1)

# random variable generation
n <- 200
ga0 <- 1.5
ga1 <- 0.5
ga2 <- -0.5
sigma <- 4
T <- c(rep(0, n/2), rep(1, n/2))
S <- runif(n, 0, 5)
eta <- ga0 + ga1 * T + ga2 * S
mu <- exp(eta)/(1+exp(eta))
Y <- rep(0, n)
for (i in 1:n){ 
  Y[i] <- rsimplex(1, mu[i], sigma)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{random variable}
