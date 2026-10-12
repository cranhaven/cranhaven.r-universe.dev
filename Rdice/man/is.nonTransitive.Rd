\name{is.nonTransitive}
\alias{is.nonTransitive}
\title{
Checks truth value of non-transitive sets of dice.
}
\description{
Checks whether a given set of dice is non-transitive with given probability. If no probability is given, checks whether a given set of dice is generally non-transitive.
}
\usage{
is.nonTransitive(df, prob)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ A \code{data.frame} containing the set of dice to be checked.}
  \item{prob}{ The non-transitive probability according to which to check for non-transitivity. If unspecified, the function checks for general non-transitivity.}
}

\value{
A logical value: TRUE or FALSE.
}
\seealso{
See also \code{\link{nonTransitive.generator}}.
}
\examples{
df <- data.frame(
  die1 = c(5,4,6,15),
  die2 = c(3,6,16,2),
  die3 = c(0,2,14,15),
  die4 = c(8,10,1,9)
)

is.nonTransitive(df, prob = 9/16)
}
