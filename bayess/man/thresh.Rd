\name{thresh}
\alias{thresh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bound for the accept-reject algorithm in Chapter 5%%  ~~function to do ... ~~
}
\description{
This function is used in \code{ardipper} to determine the bound for
the accept-reject algorithm simulating the non-standard conditional distribution
of \eqn{r_1}.%
}
\usage{
thresh(k, n1, c2, c3, r2, q1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
current proposal for the number of individuals vanishing
between the first and second experiments%%     ~~Describe \code{k} here~~
}
  \item{n1}{
first capture population size%%     ~~Describe \code{n1} here~~
}
  \item{c2}{
number of individuals recaptured during the second experiment%%     ~~Describe \code{c2} here~~
}
  \item{c3}{
number of individuals recaptured during the third experiment%%     ~~Describe \code{c3} here~~
}
  \item{r2}{
number of individuals vanishing
between the second and third experiments%%     ~~Describe \code{r2} here~~
}
  \item{q1}{
probability of disappearing from the population%%     ~~Describe \code{q1} here~~
}
}
\details{
This upper bound is equal to
\deqn{\frac{{n_1-c_2 \choose k} {n_1-k \choose c_3+r_2}}{{\bar r\choose k}}}
}
\value{
%%  ~Describe the value returned
numerical value of the upper bound, to be compared with the uniform random draw
}
\seealso{
\code{\link{ardipper}}
}
\examples{
\dontrun{if (runif(1) < thresh(y,n1,c2,c3,r2,q1))}
}
\keyword{Dipper}
\keyword{accept-reject algorithm}
