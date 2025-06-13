\name{getEtas}
\alias{getEtas}
\alias{getEtas.Exp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Construct ingredients for probability calculation.
}
\description{
The probability of a particular outcome \eqn{j} for observation \eqn{i} is given by:
  \deqn{ F\left(\frac{\alpha_{j+1}-x_i\beta}{g\left(z_i\delta\right)}\right)-F\left(\frac{\alpha_{j}-x_i\beta}{g\left(z_i\delta\right)}\right)}{F((\alpha_(j+1)-x\beta)/g(z\delta))-F((\alpha_(j+1)-x\beta)/g(z\delta))}
where \eqn{F} is the link function, the \eqn{\alpha}s refer to threshold values and \eqn{g} is the function that describes the model for the variance. This function calculates the two inputs to the link function in the above expression given precalculated values of the mean of the latent variable given parameters and the standard deviation given parameters.  
}
\usage{
getEtas(thresholds,xb,std.dev)

getEtas.Exp(thresholds,xb_matrix,sd_matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thresholds}{Numeric matrix of dimension (number of observations * 2). Columns refer to the right and left threshold corresponding to the desired outcome.}
  \item{xb, xb_matrix}{Numeric vector/matrix of expected values of the latent variable.}
  \item{std.dev, sd_matrix}{Numeric vector/matrix of standard deviations of the error term given variables.}
}

\value{
  \item{eta_1}{Numeric vector/matrix corresponding to the right threshold.}
  \item{eta_0}{Numeric vector/matrix corresponding to the left threshold.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Nathan Carroll, \email{nathan.carroll@ur.de}
%%  ~~who you are~~
}

\seealso{
\code{\link{oglmx}}
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (x) 
%{
%  }
%}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
