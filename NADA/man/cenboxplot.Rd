\name{cenboxplot}
\alias{cenboxplot}
\title{
    Produces a censored boxplot
}
\description{
    Draws a boxplot with the highest censoring threshold 
    shown as a horizontal line.  Any statistics below this line
    are invalid are must be estimated using methods for censored data.
}
\usage{
    cenboxplot(obs, cen, group, log=TRUE, range=0, ...)
}
\arguments{
\item{obs}{
    A numeric vector of observations.
} 
\item{cen}{
    A  logical vector indicating TRUE where an observation in x is
    censored (a less-than value) and FALSE otherwise.  
}
\item{group}{
    A factor vector used for grouping `obs' into subsets (each group
    will be a separate box).
}
\item{log}{
    A TRUE/FALSE indicating if the y axis should be in log units.
    Default it TRUE.
}
\item{range}{
    This determines how far the plot whiskers extend out from the box.
    If 'range' is positive, the whiskers extend to the most extreme data
    point which is no more than 'range' times the interquartile range
    from the box. The default is zero which causes the whiskers to extend to 
    the min and max data values.
}
\item{...}{
    Additional items that get passed to \code{boxplot}.
}
} % arguments
\value{
    Returns the output of the default \code{boxplot} method.
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
    data(Golden)
    with(Golden, cenboxplot(Blood, BloodCen, DosageGroup))
}
\keyword{hplot}
