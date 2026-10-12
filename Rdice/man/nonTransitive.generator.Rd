\name{nonTransitive.generator}
\alias{nonTransitive.generator}
\title{
Non-transitive dice generator.
}
\description{
This function generates \eqn{Z} random non-transitive dice given the number faces and the corresponding non-transitive probabilities.
}
\usage{
nonTransitive.generator(dice, faces, max_value = faces, prob, error = 0.001)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dice}{ The number \eqn{Z} of non-transitive dice to generate.}
  \item{faces}{ The number of faces of each die.}
  \item{max_value}{ The maximum integer allowed as nominal value for the faces. Standard choices are usually \code{max_value = faces} (default) or \code{max_value = faces^2}.}
  \item{prob}{ The probability one wants the set of dice to be non-transitive. If unspecified, a set of dice with different non-transitive probabilities for each pairing will be generated.}
  \item{error}{ Computational error to check for machine precision equality. It defaults to 0.001: no need to be specified.}
}
\details{
The function randomly generates sets of dice and stops as soon as a non-transitive set of dice matching the specified conditions is found and only if so. As such, it might happen that it never returns any value, should the condition for non-transitive dice not be matched. One may need to manually interrupt the run (time delay is provided, one could set it as max threshold.)
}
\value{
A \code{data.table} containing the set of non-transitive dice matching the specified conditions.
}

\seealso{
See also \code{\link{is.nonTransitive}}.
}



