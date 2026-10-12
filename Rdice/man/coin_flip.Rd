\name{coin.flip}
\alias{coin.flip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coin flip
}
\description{
Simulates a coin flip.
}
\usage{
coin.flip(coins = 5, flips = 100, weights = c(0.5, 0.5), getExact)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coins}{ The number of coins to flip. If unspecified, it defaults to 5.}
  \item{flips}{ The number of flips. If missing, it defaults to 100.}
  \item{weights}{ A vector of probability weights to assign to each face of the coin; if unspecified, it defaults to a fair coin with equally likely faces. If specified, its lenght must obviously be a vector of length two whose values sum up to 1.}
  \item{getExact}{ A vector containing values to be matched \emph{exactly}, namely the function returns only those combinations containing \emph{all} the above mentioned variables. Since this is a coin flip, values must be specified in the form \code{c("H", "H", "T")} as head and tails (make sure to provide the labels in quotation marks).}
}
\details{
The function is a particular case of \code{\link{dice.roll}}, namely a roll with 2 faces and 1 die.
}
\value{
{ A table containing the frequencies for each of the two occurrences (head and tail) after the specified number of flips.}
}
\examples{
coin.flip(coins = 5, flips = 100)
}



