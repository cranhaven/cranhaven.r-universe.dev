\name{isingibbs}
\alias{isingibbs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gibbs sampler for the Ising model%%%%  ~~function to do ... ~~
}
\description{
This is the original Geman and Geman (1984) Gibbs sampler
on the Ising model that gave its name to the method. It simulates
an \eqn{n\times m}{n*m} grid from the Ising distribution.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
isingibbs(niter, n, m=n, beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{niter}{
number of iterations of the algorithm%%     ~~Describe \code{niter} here~~
}
  \item{n}{
number of rows in the grid%%     ~~Describe \code{n} here~~
}
  \item{m}{
number of columns in the grid%%     ~~Describe \code{n} here~~
}
  \item{beta}{
Ising parameter%%     ~~Describe \code{beta} here~~
}
}
\value{
\code{x}, a realisation from the Ising distribution
as a matrix of size \code{n} x \code{m}
}
\seealso{
\code{\link{isinghm}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
image(1:20,1:20,isingibbs(10,20,20,beta=0.3))
}
\references{
Geman, S. and Geman, D. (1984)
Stochastic relaxation, Gibbs distributions and the Bayesian restoration of
images. \emph{IEEE Trans. Pattern Anal. Mach. Intell.}, \bold{6}, 721--741.
}
\keyword{Ising model}
\keyword{Gibbs sampling}
