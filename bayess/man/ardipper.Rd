\name{ardipper}
\alias{ardipper}
\title{Accept-reject algorithm for the open population capture-recapture model}
\description{
This function is associated with Chapter 5 on capture-recapture model.
It simulates samples from the non-standard 
distribution on \eqn{r_1}{r1}, the number of individuals vanishing
between the first and second experiments, as expressed in (5.4)
in the book, conditional on \eqn{r_2}{r2}, the number of individuals vanishing
between the second and third experiments.
}
\usage{
ardipper(nsimu, n1, c2, c3, r2, q1)
}
\arguments{
  \item{nsimu}{number of simulations}
  \item{n1}{first capture sample size}
  \item{c2}{number of individuals recaptured during the second experiment}
  \item{c3}{number of individuals recaptured during the third experiment}
  \item{r2}{number of individuals vanishing between the second and third experiments}
  \item{q1}{probability of disappearing from the population}
}
\value{
A sample of \code{nsimu} integers
}
\examples{
ardipper(10,11,3,1,0,.1)
}
