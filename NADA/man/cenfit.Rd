\name{cenfit}
\alias{cenfit}
\title{
    Compute an ECDF for Censored Data
}
\description{
    Computes an estimate of an empirical cumulative distribution function
    (ECDF) for censored data using the Kaplan-Meier method.
}
\usage{
    cenfit(obs, censored, groups, ...)
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
    Additional items that are common to this function and the \code{survfit}
    function from the `survival' package.  See Details.
}
%\item{formula}{
%    The formula must have a \code{Cen} object as the response on the
%    left of the \code{~} operator and, if desired, terms separated by +
%    operators on the right.  One of the terms may be a \code{strata}
%    object.  For a single survival curve the \code{"~ 1"} part of the
%    formula is not required.
%}

}
\details{
    This, and related routines, are front ends to routines in the
    \code{survival} package.  Since the survival routines can not handle
    left-censored data, these routines transparently handle ``flipping" input
    data and resultant calculations.  Additionally provided are query and
    prediction methods for \code{cenfit} objects.

    There are many additional options that are supported and documented
    in \code{survfit}.  Only a few have application to the geosciences.
    However, the most important is `conf.int'.  This is the level for
    a two-sided confidence interval on the ECDF.  The default is 0.95.

    If you are using the formula interface:  The \code{censored} and
    \code{groups} parameters are not specified -- all information is provided
    via a formula as the \code{obs} parameter.  The formula must have a
    \code{Cen} object as the response on the left of the \code{~} operator and,
    if desired, terms separated by + operators on the right.  
}
\value{
    a \code{cenfit} object.
    Methods defined for \code{cenfit} objects are provided for
    \code{print}, \code{plot}, \code{lines}, \code{predict},
    \code{mean}, \code{median}, \code{sd}, \code{quantile}.

    If the input formula contained factoring groups 
    (ie., \code{cenfit(obs, censored, groups)}, individual ECDFs can be
    obtained by indexing (eg., \code{model[1]}, etc.).
}
\references{
Helsel, Dennis R. (2005).  Nondectects and Data Analysis; Statistics for
censored environmental data.  John Wiley and Sons, USA, NJ.

Dorey, F. J. and Korn, E. L. (1987).  Effective sample sizes for
confidence intervals for survival probabilities.  \emph{Statistics in
Medicine} 6, 679-87.

Fleming, T. H. and Harrington, D.P. (1984).  Nonparametric estimation of
the survival distribution in censored data.  \emph{Comm. in Statistics}
13, 2469-86.

Kalbfleisch, J. D. and Prentice, R. L. (1980).  \emph{The Statistical
Analysis of Failure Time Data.} Wiley, New York.

Link, C. L. (1984). Confidence intervals for the survival function using
Cox's proportional hazards model with covariates.  \emph{Biometrics}
40, 601-610.
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\seealso{
%    \code{\link{cenfit-methods}},
    \code{\link{survfit}},
    \code{\link{Cen}}, 
    \code{\link{plot-methods}}, 
    \code{\link{mean-methods}}, 
    \code{\link{sd-methods}}, 
    \code{\link{median-methods}}, 
    \code{\link{quantile-methods}}, 
    \code{\link{predict-methods}}, 
    \code{\link{lines-methods}}, 
    \code{\link{summary-methods}}, 
    \code{\link{cendiff}} 
}

\examples{

    # Create a Kaplan-Meier ECDF, plot and summarize it.

    data(Cadmium)

    obs      = Cadmium$Cd
    censored = Cadmium$CdCen

    mycenfit = cenfit(obs, censored) 

    plot(mycenfit)
    summary(mycenfit)
    quantile(mycenfit, conf.int=TRUE)
    median(mycenfit)
    mean(mycenfit)
    sd(mycenfit)
    predict(mycenfit, c(10, 20, 100), conf.int=TRUE)

    # With groups
    groups = Cadmium$Region

    cenfit(obs, censored, groups)
    
    # Formula interface -- no groups
    cenfit(Cen(obs, censored)) 

    # Formula interface -- with groups
    cenfit(Cen(obs, censored)~groups) 
}
\keyword{survival}
