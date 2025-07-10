\name{ros} 
\alias{ros} 
\alias{lros} 
\alias{cenros} 
\title{Regression on Order Statistics} 
\description{
    \code{ros} is an implementation of a Regression on Order Statistics
    (ROS) designed for multiply censored analytical chemistry data.

    The method assumes data contains zero to many left censored (less-than)
    values.

} 
\usage{ ros(obs, censored, forwardT="log", reverseT="exp", na.action)} 
\arguments{
        \item{obs}{
            A numeric vector of observations.  This includes both censored
	          and uncensored observations.
        } 
        \item{censored}{
	    A logical vector indicating TRUE where an observation in
	    \code{obs} is censored (a less-than value) and FALSE otherwise.
        } 
        \item{forwardT}{
	        A name of a function to use for transformation prior to performing
	        the ROS fit.  Defaults to \code{log}.
        } 
        \item{reverseT}{
	        A name of a function to use for reversing the transformation
	        after performing the ROS fit. Defaults to \code{exp}.
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
    By default, \code{ros} performs a log transformation prior to, and after
    operations over the data.  This can be changed by specifying a forward and
    reverse transformation function using the \code{forwardT} and
    \code{reverseT} parameters.  No transformation will be performed if either
    \code{forwardT} or \code{reverseT} are set to \code{NULL}. 

    The procedure first computes the Weibull-type plotting positions of
    the combined uncensored and censored observations using a formula
    designed for multiply-censored data (see \code{hc.ppoints}).
    A linear regression is formed using the plotting positions of the
    uncensored observations and their normal quantiles.  This model is
    then used to estimate the concentration of the censored observations
    as a function of their normal quantiles.  Finally, the observed
    uncensored values are combined with modeled censored values to
    corporately estimate summary statistics of the entire population. By
    combining the uncensored values with modeled censored values, this
    method is more resistant of any non-normality of errors, and reduces
    any transformation errors that may be incurred.

}

%% Function return values 
\value{
    \code{ros} returns an object of class c("ros", "lm").

    \code{print} displays a simple summary of the ROS model.
    \code{as.data.frame} converts the modeled data in a ROS model to
    a data frame. Note that this discards all linear-model information
    from the object.
} 
\references{

    Lee and Helsel (2005) Statistical analysis of environmental data
    containing multiple detection limits: S-language software for
    regression on order statistics, Computers in Geoscience vol. 31,
    pp. 1241-1248.

    Lee and Helsel (2005) Baseline models of trace elements in major
    aquifers of the United States. Applied Geochemistry vol. 20,
    pp. 1560-1570.

    Dennis R. Helsel (2005), Nondetects And Data Analysis:
    John Wiley and Sons, New York.

    Dennis R. Helsel (1990), Less Than Obvious: Statistical Methods for,
    Environmental Science and Technology,
    vol.24, no. 12, pp. 1767-1774

    Dennis R. Helsel and Timothy A. Cohn (1988), Estimation of descriptive
    statistics for multiply censored water quality data, Water Resources
    Research vol. 24, no. 12, pp.1997-2004 
} 
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
    \code{\link{splitQual}}, 
    \code{\link{predict}}, 
    \code{\link{plot}}, 
    \code{\link{ros-class}}, 
    \code{\link{ros-methods}},
    \code{\link{plot-methods}},
    \code{\link{mean-methods}},
    \code{\link{sd-methods}},
    \code{\link{quantile-methods}},
    \code{\link{median-methods}},
    \code{\link{predict-methods}},
    \code{\link{summary-methods}}
}
\examples{
    obs      = c(0.5,    0.5,   1.0,  1.5,   5.0,    10,   100)
    censored = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

    myros = ros(obs, censored) 

    plot(myros)
    summary(myros)
    mean(myros); sd(myros)
    quantile(myros); median(myros)
    as.data.frame(myros)
} 
\keyword{regression} 
