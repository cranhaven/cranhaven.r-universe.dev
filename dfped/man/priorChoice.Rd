\name{priorChoice}
\alias{priorChoice}

\title{
Decision function for the choice of variance (sigmaHI or sigmaLI) in the adaptive prior variance calibration.
}

\description{
Algorithm of the decision function for the choice of variance (sigmaHI or sigmaLI) in the adaptive prior variance calibration.
}

\usage{
priorChoice(tox, givenDose, skeletonTox, lesb) 
}

\arguments{ 
  \item{tox}{ 
    The vector of toxicity.
  }
  \item{givenDose}{
    The vector of doses given to patients so far.
  }
  \item{skeletonTox}{
    Skeleton of toxicity for the BMA bivariate CRM or the bivariate CRM model.
  }
  \item{lesb}{
    A vector containing the parameters b; (resp. 0 <- b1 < ... < bk < 1).
  }
}

\references{
    Petit, C., et al, (2016) Unified approach for extrapolation and bridging of adult information in early phase dose-finding paediatric studies, Statistical Methods in Medical Research, <doi:10.1177/0962280216671348>.

    Zhang J., Braun T., and J. Taylor. (2013) Adaptive prior variance calibration in the bayesian continual reassessment method. Stat. Med. 
}

\author{
    Artemis Toumazi \email{artemis.toumazi@gmail.com},
    Caroline Petit \email{caroline.petit@crc.jussieu.fr},
    Sarah Zohar \email{sarah.zohar@inserm.fr}
}

\seealso{
    \code{\link{doseChoice}}
}

\examples{
tox <-  c(0.10, 0.21, 0.33, 0.55, 0.76)
givenDose <- 2

skeleton_tox1 <- c(0.10, 0.21, 0.33, 0.55, 0.76)
skeleton_tox2 <- c(0.21, 0.33, 0.55, 0.76, 0.88)
skeleton_tox3 <- c(0.05, 0.10, 0.21, 0.33, 0.55)
skeleton_tox4 <- c(0.025, 0.05,0.1, 0.21, 0.33)
skeleton_tox5 <- c(0.0125, 0.025, 0.05,0.1, 0.21)

skeletonTox <-  data.frame(skeleton_tox1, skeleton_tox2, skeleton_tox3, 
                           skeleton_tox4, skeleton_tox5)

lesb <- c(0.10, 0.16, 0.23, 0.25, 0.30)
priorChoice(tox, givenDose, skeletonTox, lesb)
}


