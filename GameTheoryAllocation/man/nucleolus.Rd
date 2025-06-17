\name{nucleolus}
\alias{nucleolus}
\title{
Nucleolus}
\description{
This function gives the nucleolus described in Schmeidler (1969).}
\usage{
nucleolus(characteristic_function, game = c("profit", "cost"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{characteristic_function}{
A vector with the characteristic function \eqn{v(S)} (or \eqn{c(S)}, if we work with a cost game), according to the order of coalitions shown in \code{coalitions} function.
}
  \item{game}{
Characters to indicate if the game is a cost or profit game. The possibles values are \code{"cost"} or \code{"profit"}.
}
}

\value{
A vector with the allocation that nucleolus proposes.

}
\references{
Schmeidler, D. (1969). The nucleolus of a characteristic function game, SIAM Journal of Applied Mathematics, vol. 17, pp. 1163-1170.}
\author{
A. Saavedra-Nieves}

\examples{
characteristic_function<-c(0,0.538, 0.761, 1.742, 0.554, 0.137, 0.293, 0.343)
nucleolus(characteristic_function,game="cost")
#[1] "Nucleolus"
#      1     2 3
#  0.137 0.206 0
}

\keyword{nucleolus}% use one of  RShowDoc("KEYWORDS")
\keyword{nucleolusgt}% __ONLY ONE__ keyword per line
