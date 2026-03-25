\name{pottsgibbs}
\alias{pottsgibbs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gibbs sampler for the Potts model%%  ~~function to do ... ~~
}
\description{
This function produces one simulation of a square \code{numb} by \code{numb} grid
from a Potts distribution with four colours and a four neighbour
structure, relying on \code{niter} iterations of a standard Gibbs sampler.
}
\usage{
pottsgibbs(niter, numb, beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{niter}{
number of Gibbs iterations%%     ~~Describe \code{niter} here~~
}
  \item{numb}{
size of the square grid%%     ~~Describe \code{numb} here~~
}
  \item{beta}{
parameter of the Potts model%%     ~~Describe \code{beta} here~~
}
}
\value{
%%  ~Describe the value returned
returns a random realisation from the Potts model%%
}
\references{
Geman, S. and Geman, D. (1984)
Stochastic relaxation, Gibbs distributions and the Bayesian restoration of
images. \emph{IEEE Trans. Pattern Anal. Mach. Intell.}, \bold{6}, 721--741.
}
\seealso{
\code{\link{pottshm}}
}
\examples{
ex=pottsgibbs(100,15,.4)
image(ex)
}
\keyword{Potts model}
\keyword{Gibbs sampling}
