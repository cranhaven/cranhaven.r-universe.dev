\name{plot.plMCMC}
\alias{plot.plMCMC}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Plot paths of copies of Markov chains
}
\description{
This function plots 2d-projections of the paths of i.i.d. copies of Markov chains 
output by an MCMC algorithm and stored in an object of class \code{plMCMC} (for parallel MCMC) 
such as the one returned by, e.g., \code{\link{MCMCcopies}} or the multicore version 
\code{\link{MCMCcopies.mc}}.
}
\usage{
\method{plot}{plMCMC}(x, xax = 1, yax = 2, title = NULL, cname = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{plMCMC}, such as output from
  	\code{\link{MCMCcopies}}.}
  \item{xax}{Coordinate for the horizontal axis.}
  \item{yax}{Coordinate for the vertical axis.}
  \item{title}{The title; if \code{NULL}, then a default title is displayed.}
  \item{cname}{Coordinate base name; "var" is the default, so that coordinates
  		are named "var1", "var2", and so on.}
  \item{\dots}{Further parameters except \code{pch} which is already used, 
  			passed to \code{plot}.}
}
\details{
This function is currently limited to a 2D projection path of all the i.i.d. chains for
the two selected coordinates.
The copies of the Markov chain must be in the 3-dimensional
array \code{s$Ptheta}.}
\value{The graphic to plot.}

\references{
\itemize{
   \item Chauveau, D. and Vandekerkhove, P. (2012), 
        Smoothness of Metropolis-Hastings algorithm and application to entropy estimation.
        \emph{ESAIM: Probability and Statistics},  \bold{17}, (2013) 419--431.
        DOI: \url{http://dx.doi.org/10.1051/ps/2012004}

  \item Chauveau D. and Vandekerkhove, P. (2014),
        Simulation Based Nearest Neighbor Entropy Estimation for (Adaptive) MCMC Evaluation,
    In \emph{JSM Proceedings, Statistical Computing Section}.
    Alexandria, VA: American Statistical Association. 2816--2827.

  \item Chauveau D. and Vandekerkhove, P. (2014),
        The Nearest Neighbor entropy estimate: an adequate tool for adaptive MCMC evaluation.
        \emph{Preprint HAL} \url{http://hal.archives-ouvertes.fr/hal-01068081}.
   }
}



\author{Didier Chauveau.}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{MCMCcopies}}, \code{\link{MCMCcopies.mc}},
\code{\link{MCMCcopies.cl}}
}

\examples{
## See MCMCcopie Example
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file}
