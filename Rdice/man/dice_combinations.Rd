\name{dice.combinations}
\alias{dice.combinations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dice combinations}
\description{
Calculates all possible combinations as result of rolling a set of dice. Similar permutations are identified under the same combination and counted as many times as many occurrencies. Thee user can choose wheter to match exact values or to perform partial matches.
}
\usage{
dice.combinations(faces, dice, rolls, weights, getPartial, getExact, toSum = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{faces}{ The number of faces the dice have; if unspecified, it defaults to 6.}
  \item{dice}{ The number of dice to roll; if unspecified, it defaults to 2.}
  \item{rolls}{ The number of times to roll the die; if unspeciefid, it defaults to 5.}
  \item{weights}{ A vector of probability weights to assign to each face of the die; if unspecified, it defaults to a fair die with weights \eqn{1/N}. If specified, its lenght must obviously be equals to the number of faces and all the single weights must sum up to 1.}
  \item{getExact}{ A vector containing values to be matched \emph{exactly}, namely the function returns only those combinations containing \emph{all} the above mentioned variables.}
  \item{getPartial}{ A vector containing values to be matched \emph{partially}, namely the function returns only those combinations containing \emph{at least one} of the above mentioned variables. If missing, it defaults to c(1:faces), namemly the function returns all combinations.}
  \item{toSum}{ A logical value, defaulting to FALSE. If TRUE, the function returns the sum of the frequencies of the matches (to be used together with getExact or getPartial)}
}

\details{
The function returns an object of class \emph{diceRoll}, namely a list whose values are themselves \code{data.table} objects, in turn, so that one can directly apply any \code{data.table} function thereupon.
}
\value{
  \item{values}{ If toSum = FALSE, a list of all possible combinations rolled, together with corresponding frequencies. If toSum = TRUE, the function returns the sum of all frequencies in correspondence of matched valules.}
}
\note{
The case face = 2 corresponds to the \code{\link{coin.flip}}.
}
\seealso{
Makes use of \code{\link{dice.roll}}.
}
\examples{
dice.combinations(faces = 6, dice = 4, rolls = 100,
    getExact = c(3,5), getPartial = c(1,2), toSum = TRUE)
}



