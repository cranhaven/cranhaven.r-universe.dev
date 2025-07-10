\name{cenreg}
\alias{cenreg}
\title{
    Compute regression equations and likelihood correlation coefficient
for censored data.
}
\description{
    Computes regression equations for singly censored data using maximum
    likelihood estimation. Estimates of slopes and intercept, tests
    for significance of parameters,and predicted quantiles (Median =
    points on the line) with confidence intervals can be computed.
}
\usage{
    cenreg(obs, censored, groups, ...)
}
\arguments{
\item{obs}{
    Either a numeric vector of observations or a formula.
    See examples below.
} 
\item{censored}{
    If a formula is not specified, this should be
    a logical vector indicating TRUE where an observation in obs is
    censored (a less-than value) and FALSE otherwise.  
}
\item{groups}{
    If a formula is not specified, this should be a numeric
    or factor vector that represents the explanatory variable.
}
\item{...}{
    Additional items that are common to this function and the \code{survreg}
    function from the `survival' package.  The most important of which
    is `dist' and `conf.int'.  See Details below.
}
} %END arguments
\details{
    This routine is a front end to the \code{survreg} routine in the
    \code{survival} package.  
    
    There are many additional options that are supported and documented
    in \code{survfit}.  Only a few have relevance to the evironmental
    sciences.

    A very important option is `dist' which specifies the distributional
    model to use in the regression.  The default is `lognormal'.  

    Another important option is `conf.int'.  This is NOT an option to
    \code{survreg} but is an added feature (due to some arcane details of
    R it can't be documented above).  The `conf.int' option specifies
    the level for a two-sided confidence interval on the regression.
    The default is 0.95.  This interval will be used in when the output
    object is passed to other generic functions such as \code{mean}
    and \code{quantile}.  See Examples below.

    Also supported is a `gaussian' or a normal distribution.  The use of
    a gaussian distribution requires an interval censoring context for
    left-censored data.  Luckily, this routine automatically does this
    for you -- simply specify `gaussian' and the correct manipulations
    are done.

    If any other distribution is specified besides lognormal or gaussian, 
    the return object is a raw survreg object -- it is up to the user to
    `do the right thing' with the output (and input for that matter).

    If you are using the formula interface:  The \code{censored} and
    \code{groups} parameters are not specified -- all information is
    provided via a formula as the \code{obs} parameter.  The formula
    must have a \code{Cen} object as the response on the left of the
    \code{~} operator and, if desired, terms separated by + operators
    on the right. See examples below.

    The reported likelihood r correlation coefficient measures the linear
    association between y (groups) and x (obs), based on the difference
    in log likelihoods between the fitted model and the null model.
    Slopes and intercepts are fit by maximum likelihood.  A lognormal
    distribution is fit by default, with a normal distribution being an
    option.  Estimates of predicted values on the line can be obtained
    by specifying the values for all x variables at which y is to be
    predicted.  Requesting the median (p=0.5) will provide estimates on
    the line for a lognormal distribution.  Estimates of the mean are
    also possible, as are estimates of other percentiles.  Equations for
    confidence intervals follow those of Meeker and Escobar (1098).
}
\value{
    Returns a \code{\link{summary.cenreg}} object.
}
\references{
    Helsel, Dennis R. (2005).  Nondectects and Data Analysis; Statistics for
    censored environmental data.  John Wiley and Sons, USA, NJ.

    Meeker, W.Q. and L. A. Escobar (1998).  Statistical Methods for
    Reliability Data.  John Wiley and Sons, USA, NJ.
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
    \code{\link{Cen}},
    \code{\link{cenmle}},
    \code{\link{predict-methods}}
}
\examples{

%    with(TCEReg, cenreg(log(TCEConc), TCECen, PopDensity))

    # (examples in Chap 12 of the NADA book)
    data(TCEReg)

    # Using the formula interface
    with(TCEReg, cenreg(Cen(TCEConc, TCECen)~PopDensity))

    # Two or more explanatory variables requires the formula interface
    tcemle2 = with(TCEReg, cenreg(Cen(TCEConc, TCECen)~PopDensity+Depth))

    # Prediction of quantiles at PopDensity=5 and Depth=110
    predict(tcemle2, c(5, 110))
}
\keyword{survival}
