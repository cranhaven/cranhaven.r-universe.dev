\name{Occupancy Multi-Season}
\alias{occMS0}
\alias{occMStime}
\alias{occMS}
\alias{occMScovSite}

\title{
Multi-season occupancy estimation
}
\description{
Functions to estimate occupancy from detection/non-detection data for multiple seasons. \code{occMS} is the general purpose function; it allows for site-, season- and survey-level covariates, but it is slow. \code{occMScovSite} excludes survey-level covariates, but is fast. \code{occMStime} and \code{occMS0} are simpler and faster.
}
\usage{
occMS0(DH, occsPerSeason, ci=0.95, verify=TRUE, ...)

occMStime(DH, occsPerSeason, model=NULL, data=NULL, ci=0.95, verify=TRUE, ...)

occMS(DH, occsPerSeason, model=NULL, data=NULL, ci=0.95, verify=TRUE, ...)
             
occMScovSite(DH, occsPerSeason, model=NULL, data=NULL, ci=0.95, verify=TRUE, ...)
}
\arguments{
 \item{DH}{
a 1/0/NA matrix (or data frame) of detection histories, sites x occasions. Rows with all NAs are silently removed.
}
  \item{occsPerSeason}{
the number of survey occasions per season; either a scalar if the number of surveys is constant, or a vector with one element for each season.
}
  \item{model}{
a list of formulae symbolically defining a linear predictor for each parameter in terms of covariates. The default corresponds to an intercept-only model.
}
  \item{data}{
a data frame containing the variables in the model: one row for each season or between-season period for \code{occMStime} and one for each site for \code{occMScovSite}. Each survey covariate has one column for each occasion, and the column name must end with the occasion number (without leading zeros); eg, \code{Cov1, Cov2, ..., Cov15}.
}
  \item{ci}{
the confidence interval to use.
}
  \item{verify}{
if TRUE, the data provided will be checked.
}
  \item{...}{
other arguments passed to \code{\link{nlm}}.
}
}
\details{
\code{occMS0} implements a simple multi-season model with one parameter each for initial occupancy, colonisation, local extinction, and probability of detection, ie. a \code{psi1(.) gamma(.) epsilon(.) p(.)} model.

\code{occMStime} allows for between-season differences in colonisation, local extinction, and probability of detection, either with covariates given in \code{data} or the in-built covariates \code{.interval} (for colonisation or extinction, or \code{.season} (for detection).

\code{occMScovSite} allows for between-season differences in colonisation, local extinction, and probability of detection with the in-built covariate \code{.season} and for between-site differences with covariates defined in \code{data}.

\code{occMS} allows for survey-level covariates in addition to the above, and separate covariates for between-season colonisation and local extinction.
}
\value{
Returns an object of class \code{wiqid}, see \link{wiqid-class} for details.
}

\section{Benchmarks}{Output has been checked against output from PRESENCE (Hines 2006) v.5.5 for the \code{\link{GrandSkinks}} data set. Real values are mostly the same to 4 decimal places, though there is occasionally a discrepancy of 0.0001. AICs are the same.
}

\references{
MacKenzie, D I; J D Nichols; G B Lachman; S Droege; J A Royle; C A Langtimm. 2002. Estimating site occupancy rates when detection probabilities are less than one. \emph{Ecology} 83:2248-2255.

MacKenzie, D I; J D Nichols; A J Royle; K H Pollock; L L Bailey; J E Hines 2006. \emph{Occupancy Estimation and Modeling : Inferring Patterns and Dynamics of Species Occurrence}. Elsevier Publishing.

Hines, J. E. (2006). PRESENCE - Software to estimate patch occupancy and related parameters. SGS-PWRC. http://www.mbr-pwrc.usgs.gov/software/presence.html.

MacKenzie, D I; J D Nichols; J E Hines; et al 2003. Estimating site occupancy, colonization, and local extinction when a species is imperfectly detected.  \emph{Ecology} 84, 2200-2207.

}

\author{
Mike Meredith
}

\examples{

data(GrandSkinks)
DH <- GrandSkinks[, 1:15]

occMS0(DH, 3)
\donttest{
occMStime(DH, 3, model=list(gamma ~ .interval, epsilon~1, p~.season))
occMScovSite(DH, 3,
   model=list(psi1~habitat, gamma ~ .interval, epsilon~habitat, p~.season),
   data=GrandSkinks)
}
}
