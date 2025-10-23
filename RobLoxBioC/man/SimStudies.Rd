\name{SimStudies}
\alias{AffySimStudy}
\alias{IlluminaSimStudy}
\title{Perform Monte-Carlo Study.}
\description{
  The function \code{AffySimStudy} can be used to perform Monte-Carlo studies 
  comparing Tukey's biweight and rmx estimators for normal location and scale. 
  The function \code{IlluminaSimStudy} can be used to perform Monte-Carlo studies 
  comparing Illumina's default method - a Huber-type skipped mean and sd 
  (cf. Hampel (1985)) - and rmx estimators for normal location and scale.
  In addition, maximum likelihood (ML) estimators (mean and sd) and median and 
  MAD are computed. The comparison is based on the empirical MSE.
}
\usage{
AffySimStudy(n, M, eps, seed = 123, eps.lower = 0, eps.upper = 0.05, 
             steps = 3L, fsCor = TRUE, contD, plot1 = FALSE, 
             plot2 = FALSE, plot3 = FALSE)
IlluminaSimStudy(n, M, eps, seed = 123, eps.lower = 0, eps.upper = 0.05, 
                 steps = 3L, fsCor = TRUE, contD, plot1 = FALSE, 
                 plot2 = FALSE, plot3 = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{integer; sample size, should be at least 3.}
  \item{M}{integer; Monte-Carlo replications.}
  \item{eps}{amount of contamination in [0, 0.5].}
  \item{seed}{random seed.}
  \item{eps.lower}{used by rmx estimator.}
  \item{eps.upper}{used by rmx estimator.}
  \item{steps}{integer; steps used for estimator construction.}
  \item{fsCor}{logical; use finite-sample correction.}
  \item{contD}{object of class \code{"UnivariateDistribution"}; contaminating distribution.}
  \item{plot1}{logical; plot cdf of ideal and real distribution.}
  \item{plot2}{logical; plot 20 (or M if M < 20) randomly selected samples.}
  \item{plot3}{logical; generate boxplots of the results.}
}
\details{
Normal location and scale with mean = 0 and sd = 1 is used as ideal model (without
restriction due to equivariance).

Since there is no estimator which yields reliable results if 50 percent or more of the
observations are contaminated, we use a modification where we re-simulate all samples
including at least 50 percent contaminated data.

We use funtion \code{\link[RobLox]{rowRoblox}} for the computation of the rmx estimator.
}
\value{Data.frame including empirical MSE (standardized by sample size n) and
relMSE with respect to the rmx estimator.
}
\references{
  Affymetrix, Inc. (2002). \emph{Statistical Algorithms Description Document}.
  Affymetrix, Santa Clara.

  Hampel F.R. (1985). The breakdown points of the mean combined with some rejection
  rules. Technometrics, 27(2):95-107.  
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[RobLox]{rowRoblox}}}
\examples{
set.seed(123) # to have reproducible results for package checking

AffySimStudy(n = 11, M = 100, eps = 0.02, contD = Norm(mean = 0, sd = 3), 
             plot1 = TRUE, plot2 = TRUE, plot3 = TRUE)
IlluminaSimStudy(n = 30, M = 100, eps = 0.02, contD = Norm(mean = 0, sd = 3), 
                 plot1 = TRUE, plot2 = TRUE, plot3 = TRUE)
}
\concept{Monte-Carlo study}
\keyword{robust}
