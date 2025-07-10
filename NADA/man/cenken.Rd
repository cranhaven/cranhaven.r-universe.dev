\name{cenken}
\alias{cenken}
\title{
    Compute Kendall's tau correlation coefficient and associated line
    for censored data.  Computes the Akritas-Theil-Sen nonparametric line,
    with the Turnbull estimate of intercept.

}
\description{
    Computes Kendall's tau for singly (y only) or doubly (x and y)
    censored data.  Computes the Akritas-Theil-Sen nonparametric line, 
    with the Turnbull estimate of intercept.
}
\usage{
    cenken(y, ycen, x, xcen)
}
\arguments{
\item{y}{
    A numeric vector of observations or a formula.
} 
\item{ycen}{
    A  logical vector indicating TRUE where an observation in x is
    censored (a less-than value) and FALSE otherwise.  Can be 
    missing/omitted for the case where x is not censored.
}
\item{x}{
    A numeric vector of observations.
}
\item{xcen}{
    A logical vector indicating TRUE where an observation in y is
    censored (a less-than value) and FALSE otherwise.  
}
}
\details{
    If you are using the formula interface:  The \code{ycen}, \code{x}
    and \code{xcen} parameters are not specified -- all information is
    provided via a formula as the \code{y} parameter.  The formula must
    have a \code{Cen} object as the response on the left of the \code{~}
    operator and, if desired, terms separated by + operators on the right.
    See example below.

    Kendall's tau is a nonparametric correlation coefficient measuring
    the monotonic association between y and x.  For left-censored
    data, concordant and discordant directions between x and y are
    measured whenever possible.  So with increasing x values, a change
    in y from <1 to 10 is an increase (concordant).  A change from a
    <1 to a detected 0.5 is considered a tie, as is a <1 to a <5,
    because neither can definitively be called an increase or
    decrease.  Tie corrections are employed for the variance of the
    test statistic in order to account for the many ties when
    computing p-values. The ATS line is the slope that results in a
    Kendalls tau of 0 for correlation between the residuals, y-slope*x
    and x.  The cenken routine performs an iterative bisection search to
    find that slope.  The intercept is the median residual, where the
    median for censored data is computed using the Turnbull estimate
    for interval censored data, as implmented in the Icens contributed
    package for R.
}
\value{
    Returns tau (Kendall's tau), slope, and p-value for the regression.
}
\references{
    Helsel, Dennis R. (2005).  Nondectects and Data Analysis; Statistics for
    censored environmental data.  John Wiley and Sons, USA, NJ.

    Akritas, M.G., S. A. Murphy, and M. P. LaValley (1995).  The Theil-Sen 
    Estimator With Doubly Censored Data and Applications to Astronomy.  
    Journ. Amer. Statistical Assoc. 90, p. 170-177.
}
\examples{
    # Both y and x are censored
    # (exercise 11-1 on pg 198 of the NADA book)
    data(Golden)
    with(Golden, cenken(Blood, BloodCen, Kidney, KidneyCen))

    \dontrun{
    # x is not censored
    # (example on pg 213 of the NADA book)
    data(TCEReg)
    with(TCEReg, cenken(log(TCEConc), TCECen, PopDensity))
    # formula interface
    with(TCEReg, cenken(Cen(log(TCEConc), TCECen)~PopDensity))

    # Plotting data and the regression line
    data(DFe)
    # Recall x and y parameter positons are swapped in plot vs regression calls
    with(DFe, cenxyplot(Year, YearCen, Summer, SummerCen))    # x vs. y
    reg = with(DFe, cenken(Summer, SummerCen, Year, YearCen)) # y~x
    lines(reg)
    }
}
\keyword{regression}
