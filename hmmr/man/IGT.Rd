\name{IGT}

\docType{data}

\alias{IGT}

\title{Iowa Gambling Task data}

\description{
	
	This data set contains responses of 30 participants on the Iowa Gambling Task.
	
}

\usage{data(IGT)}

\format{
  A data frame with 3000 observations on the following variables.
  \describe{
  \item{\code{id}}{a factor with participant IDs}
  \item{\code{trial}}{a numeric vector with trial numbers}
  \item{\code{deck}}{a factor with the deck chosen on each trial (A-D)}
  \item{\code{wager}}{a factor indicating whether participants wagered low or high}
  \item{\code{win}}{a numeric variable with the amount won on each trial}
  \item{\code{loss}}{a numeric variable with the amount lost on each trial}
  \item{\code{gdeck}}{a factor indicating whether the chosen deck was good/advantageous (TRUE) or not (FALSE)}
  \item{\code{fdeck}}{a factor indicating whether the chosen deck has a relatively high frequency of losses (TRUE) or not (FALSE)}
  }
}

\source{

Konstantinidis, E. and Shanks, D.R. (2014). Don't Bet on it! Wagering as a Measure of Awareness in Decision Making under Uncertainty. \emph{Journal of Experimental Psychology: General}, 143(6), 2111-2134.

}

\examples{
data(IGT)
}

\keyword{datasets}
