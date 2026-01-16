\name{reshapeWideToLong}
\alias{reshapeWideToLong}
\alias{wideToLong}
\title{
  ~ Function: wideToLong (or reshapeWideToLong) ~
}
\description{
  This function reshapes a data frame in 'wide' format (repeated
  measurements in separate column) into a data frame in 'long' format
  (repeated measurements in the same columns).
}
\usage{
wideToLong(trajWide,times=1:(ncol(trajWide)-1))
reshapeWideToLong(trajWide,times=1:(ncol(trajWide)-1))
}

\arguments{
  \item{trajWide}{\code{[data.frame]}: a data frame in 'wide' format (repeated
    measurements in separate column). The first column has to be the
  individual indentier. All the other column should be the trajectories.}
  \item{times}{\code{[vector(numeric)]}:
    specification of the times at wich the longitudinal data have
    been measured (like ages, year, month). If times is missing, it
    takes the value \code{1:(ncol(trajWide)-1)}.}
}

\details{
  This function reshapes a data frame in 'wide' format (repeated
  measurements in separe column) into a data frame in 'long' format
  (repeated measurements in the same columns). The first column has to be the
  individual indentier. All the other column should be the trajectories.
  The missing values are removed in long format.
}

\value{
A data frame in 'long' format.
}

\author{
Christophe Genolini
}


\seealso{
  \code{\link{longToWide}}, \code{\link[=stats]{reshape}}.
}
\examples{
df3 <- data.frame(id = LETTERS[rep(1:4)], sex=c("H","F","H","F"),
    v1=rnorm(4),v2=rnorm(4),w1=rnorm(4),w2=rnorm(4))

wideToLong(df3[,c(1,3,4)])
wideToLong(df3[,c(1,5,6)])
wideToLong(df3[,c(1,3:6)])
wideToLong(df3[,c(1,3:6)],times=c(1,2,4,8))
}
