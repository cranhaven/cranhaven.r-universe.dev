\name{Shapley_value}
\alias{Shapley_value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shapley_value (Shapley Value)}
\description{
This function gives the Shapley value introduced in Shapley (1953).
}
\usage{
Shapley_value(characteristic_function, game = c("profit", "cost"))
}
\arguments{
  \item{characteristic_function}{
A vector with the characteristic function \eqn{v(S)} (or \eqn{c(S)}, if we work with a cost game), according to the order of coalitions shown in \code{coalitions} function.
}
  \item{game}{
Characters to indicate if the game is a cost or profit game. The possibles values are \code{"cost"} or \code{"profit"}.
}
}

\value{
A vector with the allocation that Shapley value (Shapley, 1953) proposes.

}
\references{
Shapley, L.S.  (1953). A value por n-person games. In H. Kuhn y A. Tucker
(eds), Contributions to the theory of games II, Vol. 28, Annals of Mathematics
Studies. Princeton University Press.}
\author{
A. Saavedra-Nieves
}
\examples{
characteristic_function<-c(1,1,2,1,2,2,2)
 
Shapley_value(characteristic_function,game="cost")
#[1] "Shapley Value"
#          1         2        3
#  0.3333333 0.3333333 1.333333
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{shapley}% use one of  RShowDoc("KEYWORDS")
\keyword{shapley_value}% __ONLY ONE__ keyword per line
