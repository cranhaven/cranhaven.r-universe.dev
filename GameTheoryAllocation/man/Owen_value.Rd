\name{Owen_value}
\alias{Owen_value}
\title{
Owen_value (Owen value)
}
\description{
This function gives the Owen value described in Owen (1977).
}
\usage{
Owen_value(characteristic_function, union, game = c("profit", "cost"))
}
\arguments{
  \item{characteristic_function}{
A vector with the characteristic function \eqn{v(S)} (or \eqn{c(S)}, if we work with a cost game), according to the order of coalitions shown in \code{coalitions} function.
}
  \item{union}{
A list with a partition of the players set. Each element of this list is a union a priori, following the proposal of Owen.}
  \item{game}{
Characters to indicate if the game is a cost or profit game. The possibles values are \code{"cost"} or \code{"profit"}.}
}
\value{
A vector with the allocation that Owen value (Owen, 1977) proposes.
}
\references{
Owen, G. (1977). Values of games with a priori unions. Mathematical Economics and Game Theory: Essays in Honor
of Oskar Morgenstern (Eds.: O. Moeschlin
R. Hein). Springer, New York.}
\author{
A. Saavedra-Nieves
}

\examples{
characteristic_function<-c(1,1,2,1,2,2,2)
Owen_value(characteristic_function,union=list(c(1,2),c(3)),game="cost")
#[1] "Owen Value"
#     1    2   3
#  0.25 0.25 1.5

Shapley_value(characteristic_function,game="cost")
#[1] "Shapley Value"
#          1         2        3
#  0.3333333 0.3333333 1.333333
}
\keyword{owen}% use one of  RShowDoc("KEYWORDS")
\keyword{owen_value}% __ONLY ONE__ keyword per line
