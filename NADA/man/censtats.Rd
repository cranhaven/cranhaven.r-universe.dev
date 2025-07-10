\name{censtats}
\alias{censtats}
\title{
    Produces summary statistics using ROS, MLE, and K-M methods.
}
\description{
    A convenience function that produces a comparative table of
    summary statistics obtained using the \code{cenros}, \code{cenmle}
    and \code{cenfit} routines.  These methods are, Regression on
    Order Statistics (ROS), Maximum Likelihood Estimation (MLE), and
    Kaplan-Meier (K-M).
}
\usage{
    censtats(obs, censored)
}
\arguments{
\item{obs}{
    A numeric vector of observations.
} 
\item{censored}{
    A  logical vector indicating TRUE where an observation in x is
    censored (a less-than value) and FALSE otherwise.  
}
} % arguments
\value{
    A dataframe with the summary statistics.
}
\details{
    If the data do not fulfill the criteria for the application of 
    any method no summary statistics will be produced.
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
    with(DFe, censtats(Summer, SummerCen))
}
\keyword{hplot}
