\name{dice.roll}
\alias{dice.roll}
\title{Dice simulator}
\description{Simulates rolling of a set of dice.}
\usage{
dice.roll(faces, dice, rolls, weights)
}
\arguments{
  \item{faces}{ The number of faces the dice have; if unspecified, it defaults to 6.}
  \item{dice}{ The number of dice to roll; if unspecified, it defaults to 2.}
  \item{rolls}{ The number of times to roll the die; if unspeciefid, it defaults to 5.}
  \item{weights}{ A vector of probability weights to assign to each face of the die; if unspecified, it defaults to a fair die with weights \eqn{1/N}. If specified, its lenght must obviously be equals to the number of faces and all the single weights must sum up to 1.}
}
\details{
The function returns an object of class \emph{diceRoll}, namely a list whose values are themselves \code{data.table} objects, in turn, so that one can directly apply any \code{data.table} function thereupon.
}
\value{
 \item{results}{ The numerical results rolled.}
 \item{frequencies}{ A table containing each single occurrency (permutation) of results and the corresponding frequencies.}
 \item{sums_freq}{ A table containing the frequencies of the sums of the values obtained in each single roll by all the dice. A cumulative sum is provided too.}
 \item{exp_value_sum}{ The expectation value of the sum of the values obtained.}
}
\examples{
dice.roll(faces = 6, dice = 3, rolls = 5)
}




