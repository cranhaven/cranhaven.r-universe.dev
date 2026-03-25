\name{plotmix}
\alias{plotmix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical representation of a normal mixture log-likelihood%%  ~~function to do ... ~~
}
\description{
This function gives an \code{image} representation of the log-likelihood
surface of a mixture (Chapter 6) of two normal densities with means \eqn{\mu_1}{mu1}
and \eqn{\mu_2}{mu2} unknown. It first generates the random sample associated 
with the distribution.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotmix(mu1 = 2.5, mu2 = 0, p = 0.7, n = 500, plottin = TRUE, nl = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu1}{
first mean%%     ~~Describe \code{mu1} here~~
}
  \item{mu2}{
second mean%%     ~~Describe \code{mu2} here~~
}
  \item{p}{
weight of the first component%%     ~~Describe \code{p} here~~
}
  \item{n}{
number of observations%%     ~~Describe \code{n} here~~
}
  \item{plottin}{
boolean variable to plot the surface (or not)%%     ~~Describe \code{plottin} here~~
}
  \item{nl}{
number of contours%%     ~~Describe \code{nl} here~~
}
}
\details{
In this case, the parameters are identifiable: \eqn{\mu_1}{mu1}
and \eqn{\mu_2}{mu2} cannot be confused when \eqn{p}{p} is not 0.5.
Nonetheless, the log-likelihood surface in this figure often
exhibits two modes, one being close to the true value of the parameters
used to simulate the dataset and one corresponding to a reflected separation of 
the dataset into two homogeneous groups.
}
\value{
  \item{sample }{the simulated sample}
  \item{like }{the discretised representation of the log-likelihood surface}
}
\seealso{
\cite{\link{gibbsmean}},
\cite{\link{hmmeantemp}}
}
\examples{
resumix=plotmix()
}
\keyword{mixture of distributions}
\keyword{log-likelihood}
\keyword{identifiability}
\keyword{multimodality}
\keyword{label-switching}
