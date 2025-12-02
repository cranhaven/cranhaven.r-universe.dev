\name{plot.KbMCMC}
\alias{plot.KbMCMC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot sequences of estimates of Kullback distance or Entropy against iterations}

\description{
	This S3 method for \code{plot}
	plots by default sequences of estimates of 
	the Kullback distance \eqn{K(p^t,f)}
	between the (estimated) pdf of the MCMC algorithm at time \eqn{t},
	\eqn{p^t}, and the target  density \eqn{f},
	for \eqn{t=1} up to the number of iterations that have been provided/computed.
	It can also plot the first term in the Kullback distance, i.e.
	the Entropy \eqn{E_{p^t}[\log(p^t)]}. 
	Its argument is an object of class
	\code{KbMCMC} such as the one returned by, e.g., \code{\link{EntropyMCMC}}.
	}

\usage{
\method{plot}{KbMCMC}(x, Kullback = TRUE, lim = NULL, ylim = NULL, 
            new.plot = TRUE, title = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{An object of class \code{KbMCMC}, such as the one returned by
  			\code{\link{EntropyMCMC}}.}
  			
  \item{Kullback}{\code{TRUE} to plot the Kullback distance, 
  				\code{FALSE} to plot the Entropy.}
  				
  \item{lim}{for zooming over \code{1:lim} iterations only.}
  \item{ylim}{\code{y} limits, passed to \code{plot}.}
  
  \item{new.plot}{set to \code{FALSE} to add the plot to an existing plot.}
  
  \item{title}{The title; if \code{NULL}, then a default title is displayed.}
  
  \item{\dots}{Further parameters passed to \code{plot} or \code{lines}.}
  }

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

\seealso{\code{\link{EntropyMCMC}}, \code{\link{EntropyMCMC.mc}}}

\examples{
## See the EntropyMCMC Examples.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{file}
