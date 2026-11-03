\name{infants}
\Rdversion{1.7}
\alias{infants}
\docType{data}
\title{
Infant mortality data from North Carolina
}
\description{
Individual-level infant mortality data on 235,272 births in the U.S. state of North Carolina, in 2003 and 2004.
}
\usage{data(infants)}
\format{
  A data frame consisting of 235,464 observations, with the following columns:
  \describe{
    \item{\code{year}}{Year of birth; either 2003 or 2004.}
    \item{\code{race}}{A 9-level categorical variable indicating the race of the baby. See Details, below.}
    \item{\code{male}}{A binary variable; 0=female; 1=male.}
    \item{\code{mage}}{Age of the mother, years.}
    \item{\code{weeks}}{Number of completed weeks of gestation.}
    \item{\code{cignum}}{Average number of cigarettes. A value of `98' indicates smoking but unknown amount.}
    \item{\code{gained}}{Weight gained during pregnancy, lbs.}
    \item{\code{weight}}{Birth weight, grams.}
    \item{\code{death}}{A binary variable indicating death within 1st year of life; 0=alive; 1=death.}
  }
}
\details{
The data were compiled by the North Carolina State Center for Health Statistics (http://www.irss.unc.edu/).

The \code{race} variable is coded as follows:
  0 = Other non-white
  1 = White
  2 = Black
  3 = American Indian
  4 = Chinese
  5 = Japanese
  6 = Hawaiin
  7 = Filipino
  8 = Other Asian or Pacific Islander
}
\examples{
## Code to generate an aggregated dataset
##
data(infants)
##
infants$smoker  <- as.numeric(infants$cignum > 0)
infants$teen    <- as.numeric(infants$mage < 20)
infants$lowgain <- as.numeric(infants$gained < 20)
infants$early   <- as.numeric(infants$weeks < 32)
infants$lbw     <- as.numeric(infants$weight < 2500)
##
listAgg <- list(year=infants$year,
                smoker=infants$smoker,
                teen=infants$teen,
                lowgain=infants$lowgain,
                race=infants$race,
                male=infants$male,
                early=infants$early,
                lbw=infants$lbw)
infantsAgg <- aggregate(rep(1, nrow(infants)), listAgg, FUN=sum)
names(infantsAgg)[ncol(infantsAgg)] <- "N"
infantsAgg$Y <- aggregate(infants$death, listAgg, FUN=sum)$x
}
\keyword{datasets}
