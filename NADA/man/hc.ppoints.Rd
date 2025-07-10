\name{hc.ppoints} 
\alias{hc.ppoints}
\alias{hc.ppoints.uncen}
\alias{hc.ppoints.cen}
\title{Helsel-Cohn style plotting positions} 
\description{
    Helsel-Cohn style plotting positions for multiply-censored data.
}
\usage{
    hc.ppoints(obs, censored, na.action)
    hc.ppoints.uncen(obs, censored, cn, na.action)
    hc.ppoints.cen(obs, censored, cn, na.action)
}
\arguments{
        \item{obs}{
            A numeric vector of observations.  This includes both censored
	          and uncensored observations.
        } 
        \item{censored}{
            A logical vector indicating TRUE where an observation in v is
	          censored (a less-than value) and FALSE otherwise.
        }
        \item{cn}{
            An optional argument for internal-code use only.
            cn = a Cohn Numbers list (quantities described by 
            Helsel and Cohn (1988) in their formulation of the problem).
        }
        \item{na.action}{
            A function which indicates what should happen
            when the data contain \code{NA}s.  The default is set by
            the \code{na.action} setting of \code{\link{options}}, and is
            \code{\link{na.omit}} if that is unset.  
            Another possible value is \code{NULL}, no action. 
        }
} 
\details{
    The function computes Wiebull-type plotting positions of data
    containing mixed uncensored and censored data.  The formula was first
    described by Hirsch and Stedinger (1897) and latter reformulated by
    Helsel and Cohn (1988).  It assumes that censoring is left-censoring
    (less-thans).  A detailed discussion of the formulation is in Lee
    and Helsel (in press).

    Note that if the input vector `censored' is of zero length, then
    the plotting positions are calculated using \code{ppoints}.
    Otherwise, \code{hc.ppoints.uncen} and \code{hc.ppoints.cen}
    are used.

    \code{hc.ppoints.uncen} calculates plotting positions for uncensored 
    data only.

    \code{hc.ppoints.cen} calculates plotting positions for censored 
    data only.
}
\value{
    \code{hc.ppoints} returns a numeric vector of plotting positions
    which correspond to the observations in the input vector 'obs'.

    \code{hc.ppoints.uncen} returns a numeric vector of plotting positions
    which correspond to the uncensored observations in the input vector 'obs'.

    \code{hc.ppoints.cen} returns a numeric vector of plotting positions
    which correspond to the censored observations in the input vector 'obs'.
} 
\references{

    Lee and Helsel (in press), Statistical analysis of environmental data
    containing multiple detection limits: S-language software for linear
    regression on order statistics, Computers in Geoscience vol. X, pp. X-X

    Dennis R. Helsel and Timothy A. Cohn (1988), Estimation of descriptive
    statistics for multiply censored water quality data, Water Resources
    Research vol. 24, no. 12, pp.1997-2004 

    Robert M. Hirsch and Jery R. Stedinger (1987), Plotting positions
    for historical floods and their precision. Water Resources Research,
    vol. 23, no. 4, pp. 715-727.
} 
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
    \code{\link{ros}}, \code{\link{splitQual}}
}
\examples{
    obs      = c(0.5,    0.5,   1.0,  1.5,   5.0,    10,   100)
    censored = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

    hc.ppoints(obs, censored) 
} 
\keyword{regression} 
