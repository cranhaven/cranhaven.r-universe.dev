\name{pctCen} 
\alias{pctCen} 
\title{Calculate the percentage of values censored} 
\description{
    \code{pctCen} is a simple, but convenient, function that calculates
    the percentage of censored values.  
} 
\usage{pctCen(obs, censored, na.action)} 
\arguments{
        \item{obs}{
            A numeric vector of observations.  This includes both censored
	        and uncensored observations.
        } 
        \item{censored}{
            A logical vector indicating TRUE where an observation in v is
	        censored (a less-than value) and FALSE otherwise.
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
    100*(length(obs[censored])/length(obs))
}

%% Function return values 
\value{
    \code{pctCen} returns a single numeric value representing
    the percentage of values censored in the ``obs" vector.
} 
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
    \code{\link{splitQual}}, \code{\link{ros}}, 
}
\examples{
    obs      = c(0.5,    0.5,   1.0,  1.5,   5.0,    10,   100)
    censored = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

    pctCen(obs, censored) 
} 
\keyword{utilities} 
