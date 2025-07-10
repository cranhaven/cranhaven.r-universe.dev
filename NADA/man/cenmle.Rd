\name{cenmle}
\alias{cenmle}
\title{
    Regression by Maximum Likelihood Estimation for Left-censored Data
}
\description{

     Regression by Maximum Likelihood (ML) Estimation for left-censored
     ("nondetect" or "less-than") data.  This routine computes regression
     estimates of slope(s) and intercept by maximum likelihood
     when data are left-censored.  It will compute ML estimates of
     descriptive statistics when explanatory variables following the ~
     are left blank.  It will compute ML tests similar in function and
     assumptions to two-sample t-tests and analysis of variance when
     groups are specified following the ~.  It will compute regression
     equations, including multiple regression, when continuous explanatory
     variables are included following the ~.  It will compute the ML
     equivalent of analysis of covariance when both group and continuous
     explanatory variables are specified following the ~.  To avoid
     an appreciable loss of power with regression and group hypothesis
     tests, a probability plot of residuals should be checked to ensure
     that residuals from the regression model are approximately gaussian.
}
\usage{
    cenmle(obs, censored, groups, ...)
}
\arguments{
\item{obs}{
    Either a numeric vector of observations or a formula.
    See examples below.
} 
\item{censored}{
    A logical vector indicating TRUE where an observation in `obs' is
    censored (a less-than value) and FALSE otherwise.
}
\item{groups}{
    A factor vector used for grouping `obs' into subsets.
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
    \code{groups} parameters are not specified -- all information is provided
    via a formula as the \code{obs} parameter.  The formula must have a
    \code{Cen} object as the response on the left of the \code{~} operator and,
    if desired, terms separated by + operators on the right.  
    See Examples below.  
} %details
\value{
    a \code{cenmle} object.
    Methods defined for \code{cenmle} objects are provided for
    \code{mean}, \code{median}, \code{sd}.
}
\references{
Helsel, Dennis R. (2005).  Nondectects and Data Analysis; Statistics for
censored environmental data.  John Wiley and Sons, USA, NJ.
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
    \code{\link{Cen}}, 
    \code{\link{cenmle-methods}},
    \code{\link{mean-methods}}, 
    \code{\link{sd-methods}}, 
    \code{\link{median-methods}}, 
    \code{\link{quantile-methods}}, 
    \code{\link{summary-methods}} 
}

\examples{

    # Create a MLE regression object 

    data(TCEReg)

    tcemle = with(TCEReg, cenmle(TCEConc, TCECen)) 

    summary(tcemle)
    median(tcemle)
    mean(tcemle)
    sd(tcemle)
    quantile(tcemle)

    # This time specifiy a different confidence interval
    tcemle = with(TCEReg, cenmle(TCEConc, TCECen, conf.int=0.80)) 

    # Use the model's confidence interval with the quantile function
    quantile(tcemle, conf.int=TRUE)

    # With groupings
    with(TCEReg, cenmle(TCEConc, TCECen, PopDensity)) 
}
\keyword{survival}
