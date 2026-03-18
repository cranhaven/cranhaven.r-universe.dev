\name{t1t2.error}
\alias{t1t2.error}
\title{Plots Type I and Type II Error Rates}

\description{
  \code{t1t2.error} plots Type I (\eqn{\alpha}) and Type II (\eqn{\beta}) error rates using central and
  noncentral \code{t} distributions for any objects returned from one of the \code{PowerUpR} functions.
}

\usage{
  t1t2.error(object)
}

\arguments{
  \item{object}{an object returned from one of the \code{PowerUpR} functions.}
}

\examples{
  \dontrun{

    design1 <- mdes.bira2(rho2=.35, omega2=.10,
                            n=83, J=480)
    t1t2.error(design1)

  }
}
