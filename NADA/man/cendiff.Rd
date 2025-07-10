\name{cendiff}
\alias{cendiff}
\title{
    Test Censored ECDF Differences
}
\description{
    Tests if there is a difference between two or more empirical cumulative
    distribution functions (ECDF) using the \eqn{G^\rho}{G-rho} family of tests,
    or for a single curve against a known alternative.

}
\usage{
    cendiff(obs, censored, groups, ...)
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
    Additional items that are common to this function and the \code{survdiff}
    function from the `survival' package.  See Details.
}

%\item{formula}{
%    a formula expression as for other ECDF models, of the form
%    \code{Cen(obs, censored) ~ predictors}.  For a one-sample test, the
%    predictors must consist of a single \code{offset(sp)} term, where
%    \code{sp} is a vector giving the survival probability of each subject.
%    For a k-sample test, each unique combination of predictors defines a
%    subgroup.  A \code{strata} term may be used to produce a stratified test.
%    To cause missing values in the predictors to be treated as a separate
%    group, rather than being omitted, use the \code{strata} function with
%    its \code{na.group=T} argument.
%}

}

\value{
    Returns a list with the following components:

\item{n}{
    the number of subjects in each group.
}
\item{obs}{
    the weighted observed number of events in each group.  If there are
    strata, this will be a matrix with one column per stratum.
}
\item{exp}{
    the weighted expected number of events in each group.  If there are
    strata, this will be a matrix with one column per stratum.
}
\item{chisq}{
    the chisquare statistic for a test of equality.
}
\item{var}{
    the variance matrix of the test.
}
\item{strata}{
    optionally, the number of subjects contained in each stratum.
}}
\details{
    This, and related routines, are front ends to routines in the
    \code{survival} package.  Since the survival routines can not handle
    left-censored data, these routines transparently handle ``flipping" input
    data and resultant calculations.  

    This function shares the same arguments as \code{\link{survdiff}}.  The
    most important of which is \code{rho} which controls the type of test.
    With \code{rho = 0} this is the log-rank or Mantel-Haenszel test, and with
    \code{rho = 1} it is equivalent to the Peto & Peto modification of the
    Gehan-Wilcoxon test.  The default is \code{rho = 1}, or the Peto & Peto
    test.  This is the most appropriate for left-censored log-normal data.

    For the formula interface: if the right hand side of the formula consists
    only of an offset term, then a one sample test is done.  To cause missing
    values in the predictors to be treated as a separate group, rather than
    being omitted, use the \code{factor} function with its \code{exclude}
    argument.
}
\references{
    Helsel, Dennis R. (2005).  Nondectects and Data Analysis; Statistics for
    censored environmental data.  John Wiley and Sons, USA, NJ.

    Harrington, D. P. and Fleming, T. R. (1982).  A class of rank test
    procedures for censored survival data.  \emph{Biometrika} \bold{69},
    553-566.
}
\seealso{
    \code{\link{Cen}},
%    \code{\link{cendiff-methods}}, 
    \code{\link{survdiff}}
}
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\examples{

    data(Cadmium)

    obs      = Cadmium$Cd
    censored = Cadmium$CdCen
    groups   = Cadmium$Region

    # Cd differences between regions?
    cendiff(obs, censored, groups)
    
    # Same as above using formula interface
    cenfit(Cen(obs, censored)~groups) 
}
\keyword{survival}
