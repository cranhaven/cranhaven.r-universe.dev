\name{Accept_proposal}
\alias{Accept_proposal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Determine whether we accept the proposed poserior sample at one step of MCMC.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Determine whether the proposed poserior sample is accepted at one step of MCMC. 

}
\usage{
Accept_proposal(r)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
%%     ~~Describe \code{param} here~~
r is a real number calculated by the ratio of posterior distribution with a new proposed sample of parameters and the posterior distribution with the current parameters.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A bool value. If it is true, the proporsed sample gets accepted; if not, it gets rejected. }
\references{
Mengyang Gu. (2016). Robust Uncertainty Quantification and Scalable Computation for Computer Models with Massive Output. Ph.D. thesis. Duke University.
}
\author{
%%  ~~who you are~~
\packageAuthor{RobustCalibration}

Maintainer: \packageMaintainer{RobustCalibration}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
% \code{\link{log_marginal_lik}}.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")

\keyword{internal}