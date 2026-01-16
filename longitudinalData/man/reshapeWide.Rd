\name{reshapeLongToWide}
\alias{reshapeLongToWide}
\alias{longToWide}
\title{
  ~ Function: longToWide (or reshapeWide) ~
}
\description{
  This function reshapes a data frame in 'long' format (repeated
  measurements in the same column) into a data frame in 'wide' format
  (repeated measurements in separate columns). It also correct a bug of reshape.
}
\usage{
longToWide(trajLong)
reshapeLongToWide(trajLong)
}

\arguments{
  \item{trajLong}{[\code{data.frame}]: \code{data.frame} that hold the
    trajectories in long format. The data.frame has to be
    (no choice!) in the following format: the first column should be the individual
    indentifiant. The second should be the times at which the measurement
    are made and should be numeric. The third one should be the measurement.}
}

\details{
  This function reshapes a data frame in 'long' format (repeated
  measurements in the same column) into a data frame in 'wide' format
  (repeated measurements in separate columns).
}

\value{
A data frame in 'wide' format (repeated measurements in separate columns).
}

\author{
Christophe Genolini
}
\note{
  \code{longToWide} is just a 'friendly overlay' of the function
  \code{\link[=stats]{reshape}}. It also corrects a \code{reshape} bug
  (modification of the order of some trajectories value when some times
  are missing).
}


\seealso{
  \code{\link{wideToLong}}, \code{\link[=stats]{reshape}}.
}
\examples{

summary(Indometh)
longToWide(Indometh)


df2 <- data.frame(id = rep(LETTERS[1:4], rep(2,4)),
                 visit = I(rep(c("3","6"), 4)),
                 x = rnorm(4), y = runif(4),
                 sex=rep(c("H","F","H"),time=c(4,2,2)))[1:7,]
longToWide(df2[,1:3])
longToWide(df2[,c(1,2,4)])
}
