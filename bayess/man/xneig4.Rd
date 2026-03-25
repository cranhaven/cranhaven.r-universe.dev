\name{xneig4}
\alias{xneig4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Number of neighbours with the same colour%%  ~~function to do ... ~~
}
\description{
This is a basis function used in simulation algorithms on the Ising
and Potts models. It counts how many of the four neighbours
of \eqn{x_{a,b}}{\code{x[a,b]}} are of the same colour as this
pixel.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xneig4(x, a, b, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
grid of coloured pixels%%     ~~Describe \code{x} here~~
}
  \item{a}{
row index%%     ~~Describe \code{a} here~~
}
  \item{b}{
column index%%     ~~Describe \code{b} here~~
}
  \item{col}{
current or proposed colour%%     ~~Describe \code{col} here~~
}
}
\value{
%%  ~Describe the value returned
integer between 0 and 4 giving the number of neighbours with the same colour
}
\seealso{
\code{\link{pottsgibbs}},
\code{\link{sumising}}
}
\examples{
data(Laichedata)
xneig4(Laichedata,2,3,1)
xneig4(Laichedata,2,3,0)
}
\keyword{Ising model}
\keyword{Potts model}
\keyword{neighbourhood}
