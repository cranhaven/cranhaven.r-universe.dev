\name{usa_election}
\alias{usa_election}
\docType{data}
\title{Usa Election}
\description{2008 and 2016 Usa presidential election}
\usage{data("usa_election")}

\format{
  A data frame with 51 observations on the following 19 variables.
  \describe{
    \item{\code{state_id}}{a character vector}
    \item{\code{electoral_votes_obama}}{a numeric vector}
    \item{\code{electoral_votes_mccain}}{a numeric vector}
    \item{\code{votes_obama}}{a numeric vector}
    \item{\code{votes_mccain}}{a numeric vector}
    \item{\code{votes_others_08}}{a numeric vector}
    \item{\code{total_votes_08}}{a numeric vector}
    \item{\code{electoral_votes_trump}}{a numeric vector}
    \item{\code{electoral_votes_clinton}}{a numeric vector}
    \item{\code{votes_trump}}{a numeric vector}
    \item{\code{votes_clinton}}{a numeric vector}
    \item{\code{votes_others_16}}{a numeric vector}
    \item{\code{total_votes_16}}{a numeric vector}
    \item{\code{total_popolation_08}}{a numeric vector}
    \item{\code{total_citizen_08}}{a numeric vector}
    \item{\code{total_registered_08}}{a numeric vector}
    \item{\code{total_popolation_16}}{a numeric vector}
    \item{\code{total_citizen_16}}{a numeric vector}
    \item{\code{total_registered_16}}{a numeric vector}
  }
}


\source{
\url{https://www.census.gov/topics/public-sector/voting/data.html}
\url{https://www.fec.gov/introduction-campaign-finance/election-and-voting-information/}
}

\examples{
data(usa_election)
str(usa_election)
}
\keyword{datasets}
