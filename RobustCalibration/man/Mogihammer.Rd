\name{Mogihammer}
\alias{Mogihammer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
A geophysical model for the ground deformation in Kilauea. }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function produces outputs of ground deformation at given coordinates and parameters.
}
\usage{
Mogihammer(obsCoords, m, simul_type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obsCoords}{
%%     ~~Describe \code{param} here~~
spatial coordinate system.
}
  \item{m}{
%%     ~~Describe \code{param} here~~
A five dimensional input parameters. The first two are the location of the magma chamber; the third one is the depth of the chamber; the fourth one is magma storage rate; the last one is the Possion ratio, which is related to the rock properties. 
}
  \item{simul_type}{
If the simul_type is 2, it means it is for the ascending mode InSAR data; if the simul_type is 3, it means it is for the descending mode InSAR data.  }

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector of outputs for ground deformation. }
\references{

K. R. Anderson and M. P. Poland (2016), \emph{Bayesian estimation of magma supply, storage, and eroption rates using a multiphysical volcano model: Kilauea volcano, 2000-2012.}. \emph{Eath and Planetary Science Letters}, \bold{447}, 161-171.

K. R. Anderson and M. P. Poland (2017), \emph{Abundant carbon in the mantle beneath Hawaii}. \emph{Nature Geoscience}, \bold{10}, 704-708.

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