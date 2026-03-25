\name{rdirichlet}
\alias{rdirichlet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random generator for the Dirichlet distribution%%  ~~function to do ... ~~
}
\description{
This function simulates a sample from
a Dirichlet distribution on the \eqn{k}{k} dimensional simplex with
arbitrary parameters.
The simulation is based on a renormalised vector of gamma variates.
}
\usage{
rdirichlet(n = 1, par = rep(1, 2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
number of simulations%%     ~~Describe \code{n} here~~
}
  \item{par}{
parameters of the Dirichlet distribution, whose length determines
the value of \code{k}%%     ~~Describe \code{par} here~~
}
}
\value{
returns a \eqn{(n,k)}{n by k} matrix of Dirichlet simulations
}
\details{
Surprisingly, there is no default Dirichlet distribution generator in the R base
packages like \code{MASS} or \code{stats}. This function can be used in full
generality, apart from the book (Chapter 6).
}
\examples{
apply(rdirichlet(10,rep(3,5)),2,mean)
}
\keyword{Dirichlet distribution}
\keyword{random generation}
