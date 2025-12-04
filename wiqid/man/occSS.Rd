\name{Occupancy Single Season}
\alias{occSS0}
\alias{occSStime}
\alias{occSScovSite}
\alias{occSS}

\title{
Single-season occupancy estimation
}
\description{
Functions to estimate occupancy from detection/non-detection data for a single season. \code{occSS} is the general-purpose function, and \code{occSStime} provides plots of detection probability against time. \code{occSS0} and \code{occSScovSite} are faster functions for simpler models with summarized data. See \code{occSSrn} for the Royle-Nichols model for abundance-induced heterogeneity in detection probability.
}
\usage{
occSS(DH, model=NULL, data = NULL, ci=0.95, link=c("logit", "probit"), verify=TRUE, ...)

occSStime(DH, model=p~1, data=NULL, ci=0.95, plot=TRUE, link=c("logit", "probit"),
  verify=TRUE, ...)

occSS0(y, n, ci=0.95, link=c("logit", "probit"), ...)

occSScovSite(y, n, model=NULL, data = NULL, ci=0.95, link=c("logit", "probit"), ...)

}
\arguments{
 \item{DH}{
a 1/0/NA matrix (or data frame) of detection histories, sites x occasions.
}
  \item{model}{
a list of formulae symbolically defining a linear predictor for each parameter in terms of covariates. If NULL, an intercept-only model is used, ie, psi(.) p(.).
}
  \item{ci}{
the confidence interval to use.
}
  \item{data}{
a data frame containing the variables in the model. For \code{occSStime}, a data frame with a row for each survey occasion; otherwise, a row for each site. Each site covariate has one column. Each survey covariate has one column for each occasion, and the column name must end with the occasion number (without leading zeros); eg, \code{Cov1, Cov2, ..., Cov15}. All covariates should be included in \code{data}, otherwise they will be sought in enclosing environments, which may not produce what you want -- and they won't be standardised.
}
  \item{link}{
the link function to use, either logit or probit; see \link{Links}.
}
  \item{verify}{
if TRUE, the data provided will be checked.
}
  \item{plot}{
if TRUE (default), draws a plot of probability of detection vs time.
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

\code{occSS} allows for psi or p to be modelled as a logistic or probit function of site covariates or survey covariates, as specified by \code{model}. It includes a built in \code{.time} covariate which can be used for modelling p with time as a fixed effect, and \code{.Time, .Time2, .Time3} for a linear, quadratic or cubic trend. A built-in \code{.b} covariate corresponds to a behavioural effect, where detection depends on whether the species was detected on the previous occasion or not.

\code{occSStime} allows for time-varying covariates that are the same across all sites, eg, moon-phase. Time variables are built in, as for \code{occSS}. A plot of detection probability vs time is produced if \code{plot=TRUE}.

\code{occSS0} implements a simple model with one parameter for probability of occupancy and one for probability of detection, ie. a \code{psi(.) p(.)} model.

\code{occSScovSite} allows for site covariates but not for occasion or survey covariates.

Numeric covariates in \code{data} are standardised to facilitate convergence. This applies to binary covariates coded as 1/0; if this is not what you want, code these as TRUE/FALSE or as factors.

For speed, use the simplest function which will cope with your model. For example, you can run psi(.) p(.) models in \code{occSScovSite} or \code{occSS}, but \code{occSS0} is much faster.
}
\value{
Returns an object of class \code{wiqid}, see \link{wiqid-class} for details.

}

\section{Benchmarks}{Output has been checked against output from PRESENCE (Hines 2006) v.5.5 for the \code{\link{salamanders}} and \code{\link{weta}} data sets. Real values are mostly the same to 4 decimal places, though there is occasionally a discrepancy of 0.0001. AICs are the same.
}

\references{
MacKenzie, D I; J D Nichols; G B Lachman; S Droege; J A Royle; C A Langtimm. 2002. Estimating site occupancy rates when detection probabilities are less than one. \emph{Ecology} 83:2248-2255.

MacKenzie, D I; J D Nichols; A J Royle; K H Pollock; L L Bailey; J E Hines 2006. \emph{Occupancy Estimation and Modeling : Inferring Patterns and Dynamics of Species Occurrence}. Elsevier Publishing.

Hines, J. E. (2006). PRESENCE - Software to estimate patch occupancy and related parameters. SGS-PWRC. http://www.mbr-pwrc.usgs.gov/software/presence.html.

}

\author{
Mike Meredith
}
\seealso{
See the examples for the \code{\link{weta}} data set. See \code{\link{occ2sps}} for single-season two-species models and \code{\link{occMS}} for multi-season models.
}
\examples{
# The blue ridge salamanders data from MacKenzie et al (2006) p99:
data(salamanders)
occSS(salamanders)
occSStime(salamanders, p ~ .time)  # time as a fixed effect
occSStime(salamanders, p ~ .Time + .Time2)  # a quadratic time effect
occSS(salamanders, p ~ .b)

# or use the fast functions with y, n format:
y <- rowSums(salamanders)
n <- rowSums(!is.na(salamanders))
occSS0(y, n)
occSScovSite(y, n)

}
