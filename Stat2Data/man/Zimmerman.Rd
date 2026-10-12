\name{Zimmerman}
\alias{Zimmerman}
\docType{data}
\title{Stand Your Ground Simpson's Paradox }
\description{
Data from 220 cases in Florida where a "Stand your ground" defense was used.
}
\format{
  A data frame with 220 observations on the following 5 variables.
  \describe{
    \item{\code{Convicted}}{Was the defendant Convicted? (\code{No} or \code{Yes})}
    \item{\code{IndWhiteVictim}}{Was the victim white? (\code{1}=yes or \code{0}=no)}
    \item{\code{IndWhiteDefendant}}{Was the defendant white? (\code{1}=yes or \code{0}=no)}
    \item{\code{VictimRace}}{Race of the victim (\code{Minority} or \code{White})}
    \item{\code{DefendantRace}}{Race of the defendant (\code{Minority} or \code{White})}
  }
}
\details{
Inspired by the Travon Martin case, combined fatal and non-fatal cases of assault in Florida for which the defendant used the Stand Your Ground law in defense. These data show Simpson's Paradox. Race of the victim is more important than race of the defendant.
}
\source{
Data from Tampa Bay Times, male plus female cases, as of 2/8/15 -- final posted data
http://www.tampabay.com/stand-your-ground-law/nonfatal-cases
http://www.tampabay.com/stand-your-ground-law/fatal-cases
}
\keyword{datasets}
