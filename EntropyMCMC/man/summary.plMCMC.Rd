\name{summary.plMCMC}
\alias{summary.plMCMC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summarizes content of a \code{plMCMC} object 
		holding iid copies of MCMC's}

\description{
This S3 method for \code{summary} summarizes the content of an object 
of class \code{plMCMC} (for parallel MCMC) as returned by, e.g.,
\code{MCMCcopies}, containing the trajectories of iid copies of trajectories 
from a MCMC algorithm, and its associated kernel, target and proposal densities. 
}

\usage{
\method{summary}{plMCMC}(object, stats = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{object}{An object of class \code{plMCMC} as returned by, e.g.,\code{MCMCcopies}.}
  
  \item{stats}{print additional summary statistics for the variables over all chains.}
  
  \item{...}{additional arguments passed to other methods}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}

\value{Returns the object associated dimensions, the overall rate of acceptation,
	and descriptive statistics over the variable coordinates if \code{stats = TRUE}. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

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

\seealso{\code{\link{MCMCcopies}}, \code{\link{MCMCcopies.mc}}}

\examples{
## See Example for MCMCcopies
}

\keyword{file}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file}

