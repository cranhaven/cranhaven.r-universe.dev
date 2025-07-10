\name{censummary}
\alias{censummary}
\title{
    Produces basic summary statistics on censored data
}
\description{
    Produces basic, and hopefully useful, summary statistics on censored data.
}
\usage{
    censummary(obs, censored, groups)
}
\arguments{
\item{obs}{
    A numeric vector of observations.
} 
\item{censored}{
    A  logical vector indicating TRUE where an observation in x is
    censored (a less-than value) and FALSE otherwise.  
}
\item{groups}{
    A factor vector used for grouping `obs' into subsets.
}
} % END arguments
\value{
    A \code{censummary} object.
}
\references{
    Helsel, Dennis R. (2005).  Nondectects and Data Analysis; Statistics for
    censored environmental data.  John Wiley and Sons, USA, NJ.
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
%\seealso{
%    \code{\link{NADA}},
%}
\examples{
    data(DFe)
    with(DFe, censummary(Summer, SummerCen))
}
\keyword{hplot}
