\name{Royle-Nichols occupancy model}
\alias{occSSrn}
\alias{occSSrn0}
\alias{occSSrnSite}

\title{
Royle-Nichols model for single-season occupancy estimation
}
\description{
These functions implement the Royle-Nichols method (Royle & Nichols 2003) for estimation of site occupancy allowing for abundance-induced heterogeneity in detection probability. Probability of detection is modelled as a function of the number of animals available for detection, \emph{n}, and the probability of detection of an individual animal, \emph{r}. Probability of occupancy is derived as the probability that \emph{n} > 0.

Function \code{occSSrn} allows for site-specific covariates to be included in the model. \code{occSSrnSite} and \code{occSSrn0} are fast alternatives that do not require a full detection history matrix.
}
\usage{

occSSrn(DH, model=NULL, data = NULL, ci=0.95, link=c("logit", "probit"),
    verify=TRUE, ...)

occSSrn0(y, n, ci=0.95, link=c("logit", "probit"), ...)

occSSrnSite(y, n, model=NULL, data = NULL, ci=0.95, link=c("logit", "probit"), ...)

}
\arguments{
 \item{DH}{
a 1/0/NA matrix (or data frame) of detection histories, sites x occasions.
}
  \item{model}{
a list of formulae symbolically defining a linear predictor for each parameter in terms of covariates. If NULL, an intercept-only model is used, ie, lambda(.) r(.).
}
  \item{data}{
a data frame containing the variables in the model, with a row for each site. Each site covariate has one column. Each survey covariate has one column for each occasion, and the column name must end with the occasion number (without leading zeros); eg, \code{Cov1, Cov2, ..., Cov15}. All covariates should be included in \code{data}, otherwise they will be sought in enclosing environments, which may not produce what you want -- and they won't be standardised.

Note: currently only site covariates can be handled.
}
  \item{ci}{
the confidence interval to use.
}
  \item{link}{
the link function to use, either logit or probit; see \link{Links}.
}
  \item{verify}{
if TRUE, the data provided will be checked.
}
  \item{y}{
a vector with the number of detections at each site.
}
  \item{n}{
a scalar or vector with the number of visits (survey occasions) at each site.
}
  \item{...}{
other arguments passed to \code{\link{nlm}}.
}

}
\details{

Numeric covariates in \code{data} are standardised to facilitate convergence. This applies to binary covariates coded as 1/0; if this is not what you want, code these as TRUE/FALSE or as factors.

}
\value{
Returns an object of class \code{wiqid}, see \link{wiqid-class} for details.

}

\section{Benchmarks}{Output has been checked against output from PRESENCE (Hines 2006) v.6.9 for the \code{\link{weta}} data set. Real values are mostly the same to 4 decimal places, though there is occasionally a discrepancy of 0.001. AICs are the same.
}

\references{

MacKenzie, D I; J D Nichols; A J Royle; K H Pollock; L L Bailey; J E Hines 2006. \emph{Occupancy Estimation and Modeling : Inferring Patterns and Dynamics of Species Occurrence}. Elsevier Publishing.

Hines, J. E. (2006). PRESENCE - Software to estimate patch occupancy and related parameters. SGS-PWRC. http://www.mbr-pwrc.usgs.gov/software/presence.html.

Royle, J. A., Nichols, J. D. (2003) Estimating abundance from repeated presence-absence data or point counts. \emph{Ecology} 84(3) 777-790.
}

\author{
Mike Meredith
}
\seealso{
See the examples for the \code{\link{weta}} data set. See \code{\link{occ2sps}} for single-season two-species models and \code{\link{occMS}} for multi-season models.
}
\examples{
# The weta data from MacKenzie et al (2006) p116:
data(weta)
DH <- weta[, 1:5]
occSS(DH) # for comparison
occSSrn(DH)
y <- rowSums(DH, na.rm=TRUE)
n <- rowSums(!is.na(DH))
occSSrnSite(y, n, lambda ~ Browsed, data=weta)

}
