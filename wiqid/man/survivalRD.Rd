\name{Survival (RD)}
\alias{survRD}
\alias{survRDah}
\title{
Survival from mark-recapture data with robust design
}
\description{
Calculation of apparent survival and recruitment rate from data collected using Pollock's robust design, ie, with multiple capture occasions within each season, where the population is closed within each season. 

Function \code{survRDah} implements the second stage of a two-stage analysis, where abundance and recapture probability are estimated using closed-capture function for each season. 

Function \code{survRD} combines the two stages into a single maximum likelihood estimation step, using model M0 for the within-season data.

NOTE: These are preliminary attempts at coding these models and have not been properly tested or benchmarked.
}
\usage{
survRD(DH, freq=1, occsPerSeason)

survRDah(DH, freq=1, occsPerSeason, N, pStar)
}
\arguments{
  \item{DH}{
a 1/0 matrix with detection histories with a row for each animal captured and a column for each capture occasion.
}
  \item{freq}{
a scalar or a vector of length \code{nrow(DH)} with the frequency of each detection history. Negative values indicate trap losses.
}
  \item{occsPerSeason}{
the number of survey occasions per season; currently this must be scalar and the number of occasions must be the same for all seasons.
}
  \item{N}{
a vector with an element for each season giving the number of animals available for capture as estimated in the first stage of a 2-stage analysis. 
}
  \item{pStar}{
a vector with an element for each season giving the probability of recapture as estimated in the first stage of a 2-stage analysis. 
}
}

\value{
A list with elements:

  \item{phiHat }{Estimates of apparent survival for each interval between seasons.}
  \item{bHat }{Estimates of the recruitment rate for each interval.}
  \item{pStarHat }{The estimated probability of capture during each season.}
  \item{Nhat }{The estimated number of animals available for capture during each season.}
  \item{pHat }{The estimated probability of capture on one occasion for each season.}

For \code{survRDah}, the values of \code{pStarHat} and \code{Nhat} will equal the values of \code{pStar} and \code{N} input, and \code{pHat} with be NULL.
}
\references{
Kendall, Pollock, and Brownie (1995) A likelihood-based approach to capture-recapture estimation of demographic parameters under the robust design. \emph{Biometrics} 51:293-308

Kendall, Nichols, Hines (1997) Estimating temporary emigration using capture-recapture data with Pollock's robust design. \emph{Ecology} 78(2):563-578 
}
\author{
Mike Meredith
}


\examples{

data(MeadowVoles)
# Extract detection histories:
DH <- MeadowVoles[, 1:30]
freq <- MeadowVoles$freq

# With single stage maximum likelihood estimation:
survRD(DH, freq=freq, occsPerSeason=5)

# The 2-stage approach:
# Stage 1 - using the jackknife estimator to estimate N and p for each season:
MhResult <- matrix(NA, 6, 2)
colnames(MhResult) <- c("N", "p")
seasonID <- rep(1:6, each=5)
for(i in 1:6) {
  dh <- DH[, seasonID==i]
  MhResult[i, ] <- closedCapMhJK(dh)$real[, 1]
}
MhResult
# Calculate the probability of being captured at least once in the season:
pStar <- 1 - (1 - MhResult[, "p"])^5

# Stage 2 - pass N and pStar to a modified CJS estimation routine:
survRDah(DH, freq=freq, occsPerSeason=5, N=MhResult[, "N"], pStar=pStar)

}
